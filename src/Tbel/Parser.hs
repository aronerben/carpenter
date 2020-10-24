{-# LANGUAGE OverloadedStrings #-}

-- TODO split this up in Internal, so public interface is only parser, but rest can be tested
module Tbel.Parser where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar)

import Tbel.Base
import Tbel.Lexer
import Tbel.Syntax

-- Parsers 
-- Grammar-specific elaborate parsers
arithmeticExpression :: Parser Expression
arithmeticExpression = do
  expr <- (try $ fmap NFloat sfloat) <|> (fmap NInt sinteger)
  pure $ AExpr $ ArithmeticExpression expr

stringExpression :: Parser Expression
stringExpression = do
  quoteSymbol
  expr <- many alphaNumChar
  quoteSymbol
  pure $ SExpr $ StringExpression expr

expression :: Parser Expression
expression = do
  expr <- stringExpression <|> arithmeticExpression
  pure expr

row :: Parser Row
row = do
  exprs <- some expression
  pure $ Row exprs

header :: Parser Header
header = do
  header <- identifier
  pure header

tableExpression :: Parser TableExpression
tableExpression = do
  headers <- some header
  pipeSymbol
  rows <- sepBy1 row commaSymbol
  pure $ TableExpression headers rows

tableAssignment :: Parser TableAssignment
tableAssignment = do
  tableKeyword
  ident <- identifier
  eqSymbol
  tableExpr <- tableExpression
  pure $ TableAssignment ident tableExpr

statement :: Parser Statement
statement = Statement <$> tableAssignment

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

execParserManual :: Show a => Parser a -> Text -> IO ()
execParserManual p txt =
  case execParser errorBundlePretty p txt of
    Left err -> putStr err
    Right ast -> print ast

parser :: Text -> Either String Program
parser = execParserWithError program
