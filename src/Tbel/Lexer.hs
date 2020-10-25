--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Tbel.Lexer
  ( mspace
  , parens
  , braces
  , sinteger
  , integer
  , sfloat
  , identifier
  , tableKeyword
  , exprKeyword
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
import Text.Megaparsec.Char (alphaNumChar, char, hspace1, letterChar, string)
import Text.Megaparsec.Char.Lexer
  ( decimal
  , float
  , lexeme
  , signed
  , skipBlockComment
  , skipLineComment
  , space
  , symbol
  )

import Tbel.Base

-- General parsers
-- TODO THINK ABOUT HOW AND WHEN SPACING HAPPENS AND WHERE COMMENT IS ALLOWED
mspace :: Parser ()
mspace = space hspace1 (skipLineComment "--") (skipBlockComment "-*" "*-")

spacer :: Parser ()
spacer = space space1 adwad

mlexeme :: Parser a -> Parser a
mlexeme = lexeme mspace

msymbol :: Text -> Parser Text
msymbol = symbol mspace

fixedSymbol :: Text -> Parser ()
fixedSymbol = void . msymbol

keyword :: Text -> Parser Text
keyword kw = mlexeme $ string kw <* notFollowedBy alphaNumChar

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
integer = mlexeme decimal

sinteger :: Parser Integer
sinteger = signed mspace integer

sfloat :: Parser Double
sfloat = signed mspace float

identifier :: Parser Identifier
identifier =
  mlexeme $ do
    initialChar <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    pure $ Identifier $ initialChar : rest

-- Grammar-specific keyword and symbol parsers
-- TODO use template haskell to generate these off a list of keywords and symbols
-- genKeyword name = return $ FunD (mkName $ name ++ "Keyword") [Clause [] (NormalB ) []]
tableKeyword :: Parser ()
tableKeyword = fixedKeyword "table"

exprKeyword :: Parser ()
exprKeyword = fixedKeyword "expr"

eqSymbol :: Parser ()
eqSymbol = fixedSymbol "="

pipeSymbol :: Parser ()
pipeSymbol = fixedSymbol "|"

commaSymbol :: Parser ()
commaSymbol = fixedSymbol ","

quoteSymbol :: Parser ()
quoteSymbol = fixedSymbol "\""
