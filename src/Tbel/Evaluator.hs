{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Tbel.Evaluator where

import Control.Monad.State
import Data.Map as M (Map, insert, lookup)
import Data.Text (Text)
import Tbel.Base
import Tbel.Parser
import Tbel.Syntax

type AST = Program

-- Helpers
-- Semantics
eval :: AST -> State Integer ()
eval (Program int) = do
  return ()
