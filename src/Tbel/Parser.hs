{-# LANGUAGE OverloadedStrings #-}

-- TODO split this up in Internal, so public interface is only parser, but rest can be tested
module Tbel.Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

import Tbel.Base
import Tbel.Syntax

-- Parsers 
-- Grammar-specific elaborate parsers
execParser :: Parser Program -> Text -> Either String Program
execParser p text =
  case parse (p <* eof) "" text of
    Right parsed -> Right parsed
    Left failed -> Left $ errorBundlePretty failed

makeParserTest :: Parser a -> Text -> Either () a
makeParserTest p text =
  case parse (p <* eof) "" text of
    Right parsed -> Right parsed
    Left failed -> Left ()
