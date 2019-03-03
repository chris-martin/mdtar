{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Main (main) where

import Paths_mdtar (getDataFileName)

-- base
import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import System.IO
import System.Exit

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
  [ testCase "Single file, tar creation"
      do
        tarText <- readDirAsMdtarText "test/cases/single-file/expanded"
        expectedResult <- readTextFile "test/cases/single-file/tar.md"
        tarText === expectedResult
  ]

testCase :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
testCase name x = (name, withTests 1 (property x))

readTextFile :: MonadIO m => FilePath -> m LT.Text
readTextFile fp = liftIO (LT.readFile =<< getDataFileName fp)

readDirAsMdtarText :: MonadIO m => FilePath -> m LT.Text
readDirAsMdtarText fp = liftIO (MT.readDirAsMdtarText =<< getDataFileName fp)
