{-# LANGUAGE GADTs #-}

module Tbel.Syntax where

import Tbel.Base

-- Syntax as GADTs
data Program where
  Program :: [Statement] -> Program
  deriving (Show, Eq)

data Statement where
  Statement :: TableAssignment -> Statement
  -- TODO Add more Statements
  deriving (Show, Eq)

data TableAssignment where
  TableAssignment :: Identifier -> TableExpression -> TableAssignment
  deriving (Show, Eq)

data TableExpression where
  TableExpression :: [Header] -> [Row] -> TableExpression
  deriving (Show, Eq)

data Row where
  Row :: [Expression] -> Row
  deriving (Show, Eq)

data Expression where
  SExpr :: StringExpression -> Expression
  AExpr :: ArithmeticExpression -> Expression
  deriving (Show, Eq)

data StringExpression where
  StringExpression :: String -> StringExpression
  -- TODO add more value constructors (concat, substring, uppercase, etc.)
  deriving (Show, Eq)

data ArithmeticExpression where
  ArithmeticExpression :: Number -> ArithmeticExpression
  -- TODO add more value constructors (addition, sub, mul, etc.)
  deriving (Show, Eq)

data Number where
  NInt :: Integer -> Number
  NFloat :: Double -> Number
  deriving (Show, Eq)

-- Syntax-directed translation
-- Attributes
type Postion = Integer
-- Instances for testing
