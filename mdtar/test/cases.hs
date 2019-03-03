{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main (main) where

import Paths_mdtar (getDataFileName)

-- base
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import System.IO
import System.Exit

-- containers
import Data.Map (Map)

-- mdtar
import qualified Text.MarkdownTAR as MT

-- hedgehog
import Hedgehog

-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main :: IO ()
main =
  do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- checkParallel group
    when (not ok) exitFailure

group :: Group
group = Group "Text.MarkdownTAR" tests

tests :: [(PropertyName, Property)]
tests =
  [ testCase "Single file, producing mdtar format"
      do
        a <- readDirAsMdtarText "test/cases/single-file/expanded"
        b <- readTextFile "test/cases/single-file/tar.md"
        a === b
  , testCase "Single file, consuming mdtar format"
      do
        a <- readDirAsMap "test/cases/single-file/expanded"
        b <- readMdtarFileAsMap "test/cases/single-file/tar.md"
        a === b
  ]

testCase :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
testCase name x = (name, withTests 1 (property x))

readTextFile :: MonadIO m => FilePath -> m LT.Text
readTextFile fp = liftIO (LT.readFile =<< getDataFileName fp)

readDirAsMdtarText :: MonadIO m => FilePath -> m LT.Text
readDirAsMdtarText fp = liftIO (MT.readDirAsMdtarText =<< getDataFileName fp)

readDirAsMap :: MonadIO m => FilePath -> m (Map FilePath LT.Text)
readDirAsMap fp = liftIO (MT.readDirAsMap =<< getDataFileName fp)

readMdtarFileAsMap :: MonadIO m => FilePath -> m (Map FilePath LT.Text)
readMdtarFileAsMap fp = liftIO (MT.readMdtarFileAsMap =<< getDataFileName fp)
