{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, LambdaCase,
MultiParamTypeClasses, OverloadedStrings, RankNTypes,
ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Text.MarkdownTAR
  ( readDirAsMdtarText
  , readDirAsList
  , readMdtarFileAsList
  ) where

-- base

import qualified Data.Char as C
import qualified System.IO as IO

import Control.Applicative    ((<|>), many, liftA2, (*>), (<*), (<*>))
import Control.Exception      (Exception (displayException), throw)
import Control.Monad          (Monad (return), forever, mfilter, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool              (Bool, not, (&&))
import Data.Char              (Char)
import Data.Eq                (Eq, (==), (/=))
import Data.Foldable          (fold, for_)
import Data.Function          (($), (.))
import Data.Functor           ((<$>))
import Data.List              ((++), map)
import Data.Monoid            (Monoid (mempty))
import Data.Ord               (Ord)
import Data.Semigroup         (stimes, Semigroup ((<>)))
import Data.Traversable       (for)
import Prelude                ((-))
import System.IO              (IO, FilePath)
import Text.Show              (Show)

-- containers

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map)
import Data.Set (Set)

-- attoparsec

import qualified Data.Attoparsec.Text as P

import Data.Attoparsec.Text (Parser, endOfLine)

-- text

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.IO      as LT
import qualified Data.Text.Lazy.Builder as TB

import Data.Text (Text)

-- pipes et al

import qualified Pipes.Attoparsec   as Pipes
import qualified Pipes.Parse        as Pipes
import qualified Pipes.Prelude      as Pipes
import qualified Pipes.Safe.Prelude as Pipes

import Pipes      (Consumer', Pipe, Producer', (>->), await, yield)
import Pipes.Safe (MonadSafe, runSafeT)

-- filepath, directory

import qualified System.FilePath  as FS
import qualified System.Directory as FS

data Error
  = NotDirectory FilePath
  | UnrecognizedFileType FilePath
  | ContainsFence FilePath
  deriving (Eq, Show)

instance Exception Error
  where
    displayException (NotDirectory x) =
        "Not a directory: \"" ++ x ++ "\""
    displayException (UnrecognizedFileType x) =
        "The file \"" ++ x ++ "\" is neither symlink \
        \nor regular file nor directory"
    displayException (ContainsFence x) =
        "The file \"" ++ x ++ "\" contains a Markdown fence (\"```\") \
        \which cannot be included within a Markdown TAR"

readDirAsList :: FilePath -> IO [(FilePath, LT.Text)]
readDirAsList dir =
  (runSafeT . Pipes.toListM)
  (
      findFiles dir
      >->
      Pipes.mapM \x -> liftIO
        do
          content <- LT.readFile (filePathReal x)
          return (filePathAlias x, content)
  )

readMdtarFileAsList :: FilePath -> IO [(FilePath, LT.Text)]
readMdtarFileAsList fp =
    _

readDirAsMdtarText :: FilePath -> IO LT.Text
readDirAsMdtarText dir =
  runSafeT
    do
      chunks <- Pipes.toListM (findFiles dir >-> readToMarkdownTAR_n)
      return (LT.fromChunks chunks)

findFiles :: forall m. MonadIO m => FilePath -> Producer' FilePath' m ()
findFiles top =
    ifM $

          liftIO (FS.doesDirectoryExist top) |>
            do
              xs <- liftIO (FS.listDirectory top)
              findFiles' (Set.fromList (map (top </>) xs))

       || otherwise (throw (NotDirectory top))

findFiles' :: MonadIO m => Set FilePath' -> Producer' FilePath' m ()
findFiles' q = for_ (Set.minView q) \(x, q') ->

    ifM $

        -- At the moment for simplicity we're ignoring symlinks.
          liftIO (FS.pathIsSymbolicLink (filePathReal x)) |>
              findFiles' q'

       || liftIO (FS.doesFileExist (filePathReal x)) |>
            do
              yield x
              findFiles' q'

       || liftIO (FS.doesDirectoryExist (filePathReal x)) |>
            do
              xs <- liftIO (FS.listDirectory (filePathReal x))
              let q'' = Set.fromList (map (x </>) xs)
              findFiles' (Set.union q' q'')

       || otherwise (throw (UnrecognizedFileType (filePathReal x)))

data IfM m a = IfM (m Bool) (m a)

data IfM' m a =
  IfM'
    [IfM m a]  -- ^ Possibilities
    (m a)      -- ^ Action to perform if nothing matched

(|>) :: m Bool -> m a -> IfM m a
(|>) = IfM

(||) :: IfM m a -> IfM' m a -> IfM' m a
x || IfM' xs a = IfM' (x : xs) a

otherwise :: m a -> IfM' m a
otherwise = IfM' []

infixr 5 ||
infix 6 |>

-- | Select the first action from a list of alternatives.
ifM :: Monad m => IfM' m a -> m a
ifM (IfM' [] a) = a
ifM (IfM' ((IfM cond x) : xs) a) =
  do
    c <- cond
    if c then x else ifM (IfM' xs a)

data FilePath' =
  FilePath'
    { filePathReal :: FilePath
        -- ^ Where to find the file within the filesystem
    , filePathAlias :: FilePath
        -- ^ What to call the file in the mdtar output
    }
  deriving (Eq, Ord, Show)

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
                unless (T.null chunk) $
                    let
                        t' = t <> LT.fromStrict chunk
                    in
                        if LT.isInfixOf forbiddenText t'
                            then throw (ContainsFence (filePathReal x))
                            else
                              do
                                yield chunk
                                let
                                    t'' = LT.takeEnd
                                            (LT.length forbiddenText - 1)
                                            t'
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
match p =
  do
    (txt, _) <- P.match p
    return txt

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
    emptyLine = many pHorizontalSpace *> endOfLine *> return (mempty :: TB.Builder)
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
