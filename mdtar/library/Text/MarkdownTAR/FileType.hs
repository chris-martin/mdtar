{-# LANGUAGE BlockArguments #-}

module Text.MarkdownTAR.FileType where

import Text.MarkdownTAR.Error
import Text.MarkdownTAR.IfThenElse

-- base

import Control.Exception      (throw)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function          ((&), ($), (.))
import Prelude                (Bool, FilePath, return)

-- filepath, directory

import qualified System.FilePath  as FS
import qualified System.Directory as FS

---

data FileType = Link | File | Dir

---

getFileType :: MonadIO m => FilePath -> m FileType
getFileType fp = liftIO $
    ifThenElseM'
        [ ( FS.pathIsSymbolicLink fp, return Link )
        , ( FS.doesFileExist      fp, return File )
        , ( FS.doesDirectoryExist fp, return Dir  )
        ]
        do
          throw (err UnrecognizedFileType & setErrorFilePath fp)

isDir :: MonadIO m => FilePath -> m Bool
isDir = liftIO . FS.doesDirectoryExist
