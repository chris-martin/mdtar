{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, DeriveFunctor, OverloadedStrings,
RankNTypes, ScopedTypeVariables #-}

module Text.MarkdownTAR () where

-- base
import qualified Data.Char as C
import Control.Applicative ((<|>), many, liftA2)
import Control.Monad (mfilter)
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Text (Text)

-- pipes
import qualified Pipes.Prelude as Pipes
import Pipes

-- filepath
import System.FilePath as FS

-- directory
import qualified System.Directory as FS

findFiles :: MonadIO m => FilePath -> Producer' FilePath m ()
findFiles top = findFiles' (Set.singleton top)

findFiles' :: MonadIO m => Set FilePath -> Producer' FilePath m ()
findFiles' q =
    for_ (Set.minView q) \(x, q') ->
        ifM [ IfM (liftIO (FS.pathIsSymbolicLink x))
                do
                  findFiles' q'

            , IfM (liftIO (FS.doesFileExist x))
                do
                  yield x
                  findFiles' q'

            , IfM (liftIO (FS.doesDirectoryExist x))
                do
                  q'' <- listDirectory' x
                  findFiles' (Set.union q' q'')

            ] (fail "Path is neither symlink nor file nor directory")

data IfM m a = IfM (m Bool) (m a) deriving Functor

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

listDirectory' :: MonadIO m => FilePath -> m (Set FilePath)
listDirectory' dir = liftIO $
    (Set.fromList . (map ((dir ++) . (FS.pathSeparator :))))
    <$> FS.listDirectory dir

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
