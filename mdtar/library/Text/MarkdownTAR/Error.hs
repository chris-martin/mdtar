module Text.MarkdownTAR.Error where

import Control.Exception (Exception (displayException))
import Prelude (Eq, Show, Maybe (Just, Nothing), FilePath, (++))

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
