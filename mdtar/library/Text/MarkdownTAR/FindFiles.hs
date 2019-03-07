{-# LANGUAGE BlockArguments, LambdaCase #-}

-- For Pipes types
{-# LANGUAGE RankNTypes #-}

module Text.MarkdownTAR.FindFiles where

import Text.MarkdownTAR.Error
import Text.MarkdownTAR.FilePath
import Text.MarkdownTAR.FileType
import Text.MarkdownTAR.IfThenElse

-- base

import Control.Exception      (throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function          ((&), ($), (.))
import Data.Foldable          (for_)
import Prelude                (FilePath, map, (>>=))

-- containers

import qualified Data.Set as Set

import Data.Set (Set)

-- directory

import qualified System.Directory as FS

-- pipes et al

import qualified Pipes.Attoparsec   as Pipes
import qualified Pipes.Parse        as Pipes
import qualified Pipes.Prelude      as Pipes
import qualified Pipes.Safe.Prelude as Pipes

import Pipes      (Pipe, Producer', (>->), await, yield)
import Pipes.Safe (MonadSafe, runSafeT, catchP)

---

findFiles :: MonadIO m => FilePath -> Producer' FilePath' m ()
findFiles top =
    ifThenElseM (isDir top)
      do
        xs <- liftIO (FS.listDirectory top)
        findFiles' (Set.fromList (map (top </>) xs))
      do
        throw (err NotDirectory & setErrorFilePath top)

findFiles' :: MonadIO m => Set FilePath' -> Producer' FilePath' m ()
findFiles' q =

    for_ (Set.minView q) \(x, q') -> getFileType (filePathReal x) >>= \case

        -- todo: Currently for simplicity we ignore symlinks.
        Link -> findFiles' q'

        File -> do yield x; findFiles' q'

        Dir  -> do xs <- liftIO (FS.listDirectory (filePathReal x))
                   let q'' = Set.fromList (map (x </>) xs)
                   findFiles' (Set.union q' q'')
