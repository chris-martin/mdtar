{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, DeriveFunctor, LambdaCase,
MultiParamTypeClasses, OverloadedStrings, RankNTypes,
ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Text.MarkdownTAR () where

-- base
import qualified Data.Char as C
import qualified System.IO as IO
import Control.Applicative ((<|>), many, liftA2)
import Control.Monad (forever, mfilter)
import Data.Foldable (fold, for_)
import Data.Functor (($>))
import Data.Semigroup (stimes)
import Data.IORef

-- containers
import qualified Data.Set as Set
import Data.Set (Set)

-- attoparsec
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser, endOfLine)

-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Text (Text)

-- pipes
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Safe.Prelude as Pipes
import Pipes
import Pipes.Safe

-- filepath
import qualified System.FilePath as FS

-- directory
import qualified System.Directory as FS

findFiles :: MonadIO m => FilePath -> Producer' FilePath' m ()
findFiles top =

    ifM
        [ liftIO (FS.doesDirectoryExist top) |>
            do
              xs <- liftIO (FS.listDirectory top)
              findFiles' (Set.fromList (map (top </>) xs))

        ]   do
              fail ("Not a directory: \"" ++ top ++ "\"")

findFiles' :: MonadIO m => Set FilePath' -> Producer' FilePath' m ()
findFiles' q = for_ (Set.minView q) \(x, q') ->
                                                        q
    ifM
        -- At the moment for simplicity we're ignoring symlinks.
        [ liftIO (FS.pathIsSymbolicLink (filePathReal x)) |>
              findFiles' q'

        , liftIO (FS.doesFileExist (filePathReal x)) |>
            do
              yield x
              findFiles' q'

        , liftIO (FS.doesDirectoryExist (filePathReal x)) |>
            do
              xs <- liftIO (FS.listDirectory (filePathReal x))
              let q'' = Set.fromList (map (x </>) xs)
              findFiles' (Set.union q' q'')

        ]   do
              fail "Path is neither symlink nor file nor directory"

data IfM m a = IfM (m Bool) (m a) deriving Functor

(|>) = IfM

-- | Select the first action from a list of alternatives.
ifM :: Monad m
    => [IfM m a]  -- ^ Possibilities
    -> m a        -- ^ Action to perform if nothing matched
    -> m a
ifM [] a = a
ifM (IfM cond x : xs) a =
  do
    c <- cond
    if c then x else ifM xs a

data FilePath' =
  FilePath'
    { filePathReal :: FilePath
        -- ^ Where to find the file within the filesystem
    , filePathAlias :: FilePath
        -- ^ What to call the file in the mdtar output
    }
  deriving (Eq, Ord)

class PathJoin a b
  where
    (</>) :: a -> b -> FilePath'

instance PathJoin FilePath FilePath
  where
    base </> rel =
      FilePath'
        { filePathReal = base FS.</> rel
        , filePathAlias = rel
        }

instance PathJoin FilePath' FilePath'
  where
    base </> rel =
      FilePath'
        { filePathReal  = filePathReal  base FS.</> filePathReal  rel
        , filePathAlias = filePathAlias base FS.</> filePathAlias rel
        }

instance PathJoin FilePath' FilePath
  where
    base </> rel =
      FilePath'
        { filePathReal  = filePathReal  base FS.</> rel
        , filePathAlias = filePathAlias base FS.</> rel
        }

newline :: Text
newline =
  case IO.nativeNewline of
    IO.LF   -> "\n"
    IO.CRLF -> "\r\n"

readToMarkdownTAR_1 :: MonadSafe m => FilePath' -> Producer' Text m ()
readToMarkdownTAR_1 x =
  do
    yield "## "
    yield (T.pack (filePathAlias x))
    yield newline
    yield newline
    yield "```"
    yield newline

    Pipes.withFile (filePathReal x) IO.ReadMode \h ->
        let
            go t =
              do
                chunk <- liftIO (T.hGetChunk h)

                let t' = t <> LT.fromStrict chunk

                if LT.isInfixOf forbiddenText t'
                  then fail "A file including a Markdown fence (\"```\") \
                            \cannot be included within a Markdown TAR"
                  else
                    do
                      yield chunk
                      let t'' = LT.takeEnd (LT.length forbiddenText - 1) t'
                      go t''
        in
            go "\n"

    yield "```"
    yield newline

  where
    forbiddenText = "\n```"

readToMarkdownTAR_n :: MonadSafe m => Pipe FilePath' Text m ()
readToMarkdownTAR_n =
  do
    x <- await
    readToMarkdownTAR_1 x
    forever
      do
        x <- await
        yield newline
        readToMarkdownTAR_1 x

buildText :: TB.Builder -> Text
buildText = LT.toStrict . TB.toLazyText

(<+>) :: Semigroup a => Parser a -> Parser a -> Parser a
(<+>) = liftA2 (<>)

foldMany :: Monoid a => Parser a -> Parser a
foldMany p = fold <$> many p

match :: Parser a -> Parser Text
match p = fst <$> P.match p

pCode :: Parser Text
pCode =

    pCodeFenced <|> pCodeIndented

pCodeFenced :: Parser Text
pCodeFenced =

    open *> code <* close

  where
    fence = "```"

    open = P.string fence *> P.skipWhile (not . P.isEndOfLine) *> endOfLine
    close = P.string fence *> P.skipMany pHorizontalSpace *> endOfLine

    code = match (P.skipMany lineOfCode)

    lineOfCode =
        mfilter (not . (fence `T.isPrefixOf`))
                (P.takeWhile (not . P.isEndOfLine))
        <* endOfLine

pCodeIndented :: Parser Text
pCodeIndented =

  do
    i <- pIndentation
    firstLine <- takeLine
    subsequentLines <- foldMany $
        foldMany emptyLine <+> (P.string i *> takeLine)
    return (buildText (firstLine <> subsequentLines))

  where
    emptyLine = many pHorizontalSpace *> endOfLine $> (mempty :: TB.Builder)
    takeLine = TB.fromText <$> match (P.skipWhile (not . P.isEndOfLine) *> endOfLine)

pIndentation :: Parser Text
pIndentation =

    P.string (stimes 4 " ") <|> P.string "\t"

pPath :: Parser Text
pPath =

    pPathHash <|> pPathUnderline

pPathHash :: Parser Text
pPathHash =

    hashes *> path <* endOfLine

  where
    hashes = P.string "##" *> mfilter (/= '#') P.peekChar'
    path = T.strip <$> P.takeWhile1 (not . P.isEndOfLine)

pPathUnderline :: Parser Text
pPathUnderline =

    text <* endOfLine <* underline <* trailingWhitespace <* endOfLine

  where
    text = T.strip <$> P.takeWhile1 (not . P.isEndOfLine)
    underline = P.skipMany1 (P.char '-')
    trailingWhitespace = P.skipMany pHorizontalSpace

pHorizontalSpace :: Parser Char
pHorizontalSpace =

    P.satisfy (\c -> C.isSpace c && not (P.isEndOfLine c))
