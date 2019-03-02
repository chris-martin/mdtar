module Main (main) where

import Paths_mdtar (getDataFileName)

import qualified System.Directory as Dir

main :: IO ()
main =
  do
    casesDir <- getDataFileName "test/cases"
    caseDirs <- Dir.listDirectory casesDir
    print caseDirs
