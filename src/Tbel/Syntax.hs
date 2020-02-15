{-# LANGUAGE GADTs #-}

module Tbel.Syntax where

import Tbel.Base

-- Syntax as GADTs
data Program where
  Program :: Integer -> Program
  deriving (Show, Eq)

-- Syntax-directed translation
-- Attributes
type Postion = Integer
-- Instances for testing
