{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, RankNTypes,
             ScopedTypeVariables #-}

module Text.MarkdownTAR
  ( readDirAsMdtarText
  , readDirAsList
  , readMdtarFileAsList
  ) where

import Text.MarkdownTAR.Attoparsec
import Text.MarkdownTAR.FilePath

-- base

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

    ifM [ifDir] fail

  where

    ifDir =
      liftIO (FS.doesDirectoryExist top) |>
        do
          xs <- liftIO (FS.listDirectory top)
          findFiles' (Set.fromList (map (top </>) xs))

    fail = throw (NotDirectory top)

findFiles' :: MonadIO m => Set FilePath' -> Producer' FilePath' m ()
findFiles' q =

    for_ (Set.minView q) f

  where
    f (x, q') = ifM [ifLink, ifFile, ifDir] fail

      where
        -- At the moment for simplicity we're ignoring symlinks.
        ifLink =
          liftIO (FS.pathIsSymbolicLink (filePathReal x)) |>
              findFiles' q'

        ifFile =
          liftIO (FS.doesFileExist (filePathReal x)) |>
            do
              yield x
              findFiles' q'

        ifDir =
          liftIO (FS.doesDirectoryExist (filePathReal x)) |>
            do
              xs <- liftIO (FS.listDirectory (filePathReal x))
              let q'' = Set.fromList (map (x </>) xs)
              findFiles' (Set.union q' q'')

        fail = throw (UnrecognizedFileType (filePathReal x))

data IfM m a = IfM (m Bool) (m a)

(|>) :: m Bool -> m a -> IfM m a
(|>) = IfM

-- | Select the first action from a list of alternatives.
ifM :: Monad m => [IfM m a] -> m a -> m a
ifM ifs orElse =

    go ifs

  where

    go [] = orElse
    go ((IfM cond x) : xs) =
      do
        c <- cond
        if c then x else go xs

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
