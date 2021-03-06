{-# LANGUAGE OverloadedStrings #-}

-- TODO split this up in Internal, so public interface is only parser, but rest can be tested
module Carpenter.Parser where

import Carpenter.Base
import Carpenter.Lexer
import Carpenter.Syntax
import Data.Char (isPrint)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (asciiChar, newline)

-- Parsers
-- Grammar-specific elaborate parsers
arithmeticExpression :: Parser Expression
arithmeticExpression = do
  expr <- try (NFloat <$> sfloat) <|> (NInt <$> sinteger)
  pure $ AExpr $ ArithmeticExpression expr

stringExpression :: Parser Expression
stringExpression = do
  quoteSymbol
  expr <-
    takeWhileP (Just "ascii character") (\chr -> isPrint chr && chr /= '"')
  quoteSymbol
  pure $ SExpr $ StringExpression expr

expression :: Parser Expression
expression = stringExpression <|> arithmeticExpression

row :: Parser Row
row = Row <$> some expression

header :: Parser Header
header = identifier

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
  TableAssignment ident <$> tableExpression

exprAssigment :: Parser ExpressionAssignment
exprAssigment = do
  exprKeyword
  ident <- identifier
  eqSymbol
  ExpressionAssignment ident <$> expression

statement :: Parser Statement
statement =
  (TStatement <$> tableAssignment <|> EStatement <$> exprAssigment)
    <* semicolonSymbol

program :: Parser Program
program =
  do
    mspace -- Ignore possible spaces/comments in the beginning
    stmts <- many statement
    pure $ Program stmts
    <* eof -- Parse til eof

-- Parser starters
execParser ::
  (ParseErrorBundle Text Void -> b) -> Parser a -> Text -> Either b a
execParser errFn p text =
  case parse p "" text of
    Right parsed -> Right parsed
    Left failed -> Left $ errFn failed

execParserTest :: Parser a -> Text -> Either () a
execParserTest = execParser $ const ()

execParserManual :: Show a => Parser a -> Text -> IO ()
execParserManual p txt =
  case execParser errorBundlePretty p txt of
    Left err -> putStr err
    Right ast -> print ast

execParserWithError :: Parser Program -> Text -> Either String Program
execParserWithError = execParser errorBundlePretty

parser :: Text -> Either String Program
parser = execParserWithError program
