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
import Text.MarkdownTAR.Error
import Text.MarkdownTAR.FilePath
import Text.MarkdownTAR.FindFiles

-- base

import qualified System.IO as IO

import Control.Exception      (throw)
import Control.Monad          (Monad, return, forever, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function          (($), (.))
import Data.Semigroup         (Semigroup ((<>)))
import Prelude                ((-))
import System.IO              (IO, FilePath)

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

-- directory

import qualified System.Directory as FS

---

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
