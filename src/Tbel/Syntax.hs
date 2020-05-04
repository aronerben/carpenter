{-# LANGUAGE GADTs #-}

module Tbel.Syntax where

import Tbel.Base

-- Syntax as GADTs
data Program where
  Program :: [Statement] -> Program
  deriving (Show, Eq)

data Statement where
  Statement :: TableAssignment -> Statement
  deriving (Show, Eq)

data TableAssignment where
  TableAssignment :: Identifier -> TableExpression -> TableAssignment
  deriving (Show, Eq)

data TableExpression where
  TableExpression :: Headers -> Row -> TableExpression
  deriving (Show, Eq)

data Row where
  SRow :: StringExpression -> Row
  ARow :: ArithmeticExpression -> Row
  deriving (Show, Eq)

data StringExpression where
  StringExpression :: String -> StringExpression
  -- TODO add more value constructors (concat, substring, uppercase, etc.)
  deriving (Show, Eq)

data ArithmeticExpression where
  ArithmeticExpression :: Double -> ArithmeticExpression
  -- TODO make this a generic Num (implement Show and Eq instance)
  -- TODO add more value constructors (addition, sub, mul, etc.)
  deriving (Show, Eq)

-- Syntax-directed translation
-- Attributes
type Postion = Integer
-- Instances for testing
