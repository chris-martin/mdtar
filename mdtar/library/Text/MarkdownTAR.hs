{-# LANGUAGE OverloadedStrings #-}

module Text.MarkdownTAR where

import qualified Data.Char as C
import Control.Applicative ((<|>))
import Control.Monad (mfilter)

import Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)

import qualified Data.Text as T
import Data.Text (Text)

pCodeBlock :: Parser Text
pCodeBlock =

    open *> code <* close

  where
    fence = "```"

    open = P.string fence *> P.skipWhile (not . P.isEndOfLine) *> P.endOfLine
    close = P.string fence *> P.skipMany pHorizontalSpace *> P.endOfLine

    code = fst <$> P.match (P.skipMany lineOfCode)

    lineOfCode =
        mfilter (not . (fence `T.isPrefixOf`))
                (P.takeWhile (not . P.isEndOfLine))
        <* P.endOfLine

pPath :: Parser Text
pPath =

    pPathHash <|> pPathUnderline

pPathHash :: Parser Text
pPathHash =

    hashes *> path <* eol

  where
    hashes = P.string "##" *> mfilter (/= '#') peekChar'
    path = T.strip <$> P.takeWhile1 (not . P.isEndOfLine)
    eol = P.endOfLine

pPathUnderline :: Parser Text
pPathUnderline =

    text <* eol <* underline <* trailingWhitespace <* eol

  where
    text = T.strip <$> P.takeWhile1 (not . P.isEndOfLine)
    eol = P.endOfLine
    underline = P.skipMany1 (P.char '-')
    trailingWhitespace = P.skipMany pHorizontalSpace

pHorizontalSpace :: Parser Char
pHorizontalSpace =

    P.satisfy (\c -> C.isSpace c && not (P.isEndOfLine c))
