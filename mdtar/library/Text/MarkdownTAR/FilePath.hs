-- Extensions for the PathJoin class and its instances
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Text.MarkdownTAR.FilePath where

-- base

import qualified System.IO as IO

-- filepath, directory

import qualified System.FilePath  as FS
import qualified System.Directory as FS

-- text

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.IO      as LT
import qualified Data.Text.Lazy.Builder as TB

import Data.Text (Text)

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
    T.pack $
        case IO.nativeNewline of
            IO.LF   -> "\n"
            IO.CRLF -> "\r\n"
