{-# LANGUAGE OverloadedStrings #-}

-- TODO split this up in Internal, so public interface is only parser, but rest can be tested
module Tbel.Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

import Tbel.Base
import Tbel.Lexer
import Tbel.Syntax

-- Parsers 
-- Grammar-specific elaborate parsers
tblassign :: Parser TableAssignment
-- TODO continue here
tblassign = tblassign

statement :: Parser Statement
statement = Statement <$> tblassign

program :: Parser Program
program = do
  stmts <- many statement
  pure $ Program stmts

-- Parser starters
execParser ::
     (ParseErrorBundle Text Void -> b) -> Parser a -> Text -> Either b a
execParser errFn p text =
  case parse (p <* eof) "" text of
    Right parsed -> Right parsed
    Left failed -> Left $ errFn failed

execParserWithError :: Parser Program -> Text -> Either String Program
execParserWithError = execParser errorBundlePretty

execParserTest :: Parser a -> Text -> Either () a
execParserTest = execParser $ const ()

parser :: Text -> Either String Program
parser = execParserWithError program
