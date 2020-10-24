--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tbel.Lexer
  ( parens
  , braces
  , sinteger
  , integer
  , sfloat
  , identifier
  , tableKeyword
  , eqSymbol
  , pipeSymbol
  , commaSymbol
  , quoteSymbol
  ) where

import Control.Monad (void)
import Control.Monad.Combinators (some)
import Control.Monad.Combinators.Expr
import Data.Text (Text)

--import Language.Haskell.TH
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as Lex

import Tbel.Base

-- General parsers
space :: Parser ()
space =
  Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "-*" "*-")

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space

fixedSymbol :: Text -> Parser ()
fixedSymbol = void . symbol

keyword :: Text -> Parser Text
keyword kw = lexeme $ string kw <* notFollowedBy alphaNumChar

fixedKeyword :: Text -> Parser ()
fixedKeyword = void . keyword

betweenH :: Text -> Text -> Parser a -> Parser a
betweenH left right = between (fixedSymbol left) (fixedSymbol right)

-- Grammar-specific basic element parsers
parens :: Parser a -> Parser a
parens = betweenH "(" ")"

braces :: Parser a -> Parser a
braces = betweenH "{" "}"

integer :: Parser Integer
integer = lexeme Lex.decimal

sinteger :: Parser Integer
sinteger = Lex.signed space integer

sfloat :: Parser Double
sfloat = Lex.signed space Lex.float

identifier :: Parser Identifier
identifier =
  lexeme $ do
    initialChar <- letterChar
    rest <- many alphaNumChar
    pure $ Identifier $ initialChar : rest

-- Grammar-specific keyword and symbol parsers
-- TODO use template haskell to generate these off a list of keywords and symbols
-- genKeyword name = return $ FunD (mkName $ name ++ "Keyword") [Clause [] (NormalB ) []]
tableKeyword :: Parser ()
tableKeyword = fixedKeyword "table"

eqSymbol :: Parser ()
eqSymbol = fixedSymbol "="

pipeSymbol :: Parser ()
pipeSymbol = fixedSymbol "|"

commaSymbol :: Parser ()
commaSymbol = fixedSymbol ","

quoteSymbol :: Parser ()
quoteSymbol = fixedSymbol "\""
