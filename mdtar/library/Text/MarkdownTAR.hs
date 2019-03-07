{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-} -- todo: temporary for development

{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, RankNTypes,
             ScopedTypeVariables #-}

module Text.MarkdownTAR
  ( readDirAsMdtarText
  , readDirAsList
  , readMdtarFileAsList
  ) where

import Text.MarkdownTAR.Attoparsec
import Text.MarkdownTAR.FilePath
import Text.MarkdownTAR.IfThenElse


-- base

import qualified System.IO as IO

import Control.Exception      (Exception (displayException), throw)
import Control.Monad          (Monad, return, (>>=), forever, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool              (Bool)
import Data.Eq                (Eq)
import Data.Foldable          (for_)
import Data.Function          (($), (.), (&))
import Data.List              ((++), map)
import Data.Maybe             (Maybe (Nothing, Just))
import Data.Semigroup         (Semigroup ((<>)))
import Prelude                ((-))
import System.IO              (IO, FilePath)
import Text.Show              (Show)

-- containers

import qualified Data.Set as Set

import Data.Set (Set)

-- text

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.IO      as LT

import Data.Text (Text)

-- pipes et al

import qualified Pipes.Attoparsec   as Pipes
import qualified Pipes.Parse        as Pipes
import qualified Pipes.Prelude      as Pipes
import qualified Pipes.Safe.Prelude as Pipes

import Pipes      (Pipe, Producer', (>->), await, yield)
import Pipes.Safe (MonadSafe, runSafeT, catchP)

-- filepath, directory

import qualified System.FilePath  as FS
import qualified System.Directory as FS

data Error = Error ErrorType ErrorDetail
  deriving (Eq, Show)

data ErrorType = NotDirectory | UnrecognizedFileType | ContainsFence
  deriving (Eq, Show)

data ErrorDetail = ErrorDetail { errorFilePath :: Maybe FilePath }
  deriving (Eq, Show)

err :: ErrorType -> Error
err t = Error t (ErrorDetail Nothing)

setErrorFilePath :: FilePath -> Error -> Error
setErrorFilePath x (Error t d) = Error t d{ errorFilePath = Just x }

instance Exception Error
  where
    displayException = displayError

displayError (Error t d) =
  case t of

    NotDirectory ->
      case d of
        ErrorDetail { errorFilePath = Just x } ->
            "Not a directory: \"" ++ x ++ "\""
        ErrorDetail { errorFilePath = Nothing } ->
            "Not a directory"

    UnrecognizedFileType ->
      case d of
        ErrorDetail { errorFilePath = Just x } ->
          "The file \"" ++ x ++ "\" is neither symlink \
          \nor regular file nor directory"
        ErrorDetail { errorFilePath = Nothing } ->
          "File is neither symlink nor regular file nor directory"

    ContainsFence ->
      case d of
        ErrorDetail { errorFilePath = Just x } ->
          "The file \"" ++ x ++ "\" contains a Markdown fence (\"```\") \
          \which cannot be included within a Markdown TAR"
        ErrorDetail { errorFilePath = Nothing } ->
          "A Markdown fence (\"```\") cannot be included \
          \within a Markdown TAR"

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
  do
    _ pPath
    _ pCode

readDirAsMdtarText :: FilePath -> IO LT.Text
readDirAsMdtarText dir =
  runSafeT
    do
      chunks <- Pipes.toListM (findFiles dir >-> readToMarkdownTAR_n)
      return (LT.fromChunks chunks)

isDir :: MonadIO m => FilePath -> m Bool
isDir = liftIO . FS.doesDirectoryExist

findFiles :: forall m. MonadIO m => FilePath -> Producer' FilePath' m ()
findFiles top =
    ifThenElseM (isDir top)
      do
        xs <- liftIO (FS.listDirectory top)
        findFiles' (Set.fromList (map (top </>) xs))
      do
        throw (err NotDirectory & setErrorFilePath top)

data FileType = Link | File | Dir

getFileType :: MonadIO m => FilePath -> m FileType
getFileType fp = liftIO $
    ifThenElseM'
        [ ( FS.pathIsSymbolicLink fp, return Link )
        , ( FS.doesFileExist      fp, return File )
        , ( FS.doesDirectoryExist fp, return Dir  )
        ]
        do
          throw (err UnrecognizedFileType & setErrorFilePath fp)

findFiles' :: MonadIO m => Set FilePath' -> Producer' FilePath' m ()
findFiles' q =

    for_ (Set.minView q) \(x, q') -> getFileType (filePathReal x) >>= \case

        -- todo: Currently for simplicity we ignore symlinks.
        Link -> findFiles' q'

        File -> do yield x; findFiles' q'

        Dir  -> do xs <- liftIO (FS.listDirectory (filePathReal x))
                   let q'' = Set.fromList (map (x </>) xs)
                   findFiles' (Set.union q' q'')

hText :: MonadIO m => IO.Handle -> Producer' Text m ()
hText h =
    go
  where
    go =
      do
        chunk <- liftIO (T.hGetChunk h)
        unless (T.null chunk) $
          do
            yield chunk
            go

forbidFence :: Monad m => Pipe Text Text m ()
forbidFence =

    go "\n"

  where
    go t =
      do
        chunk <- await
        let t' = t <> LT.fromStrict chunk
        if LT.isInfixOf forbiddenText t'
            then throw (err ContainsFence)
            else
              do
                yield chunk
                let
                    t'' = LT.takeEnd
                            (LT.length forbiddenText - 1)
                            t'
                go t''

    forbiddenText = "\n```"

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
          (hText h >-> forbidFence)

      yield "```"
      yield newline

  `catchP` (throw . setErrorFilePath (filePathReal x))

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
