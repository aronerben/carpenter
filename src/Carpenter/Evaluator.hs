{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Carpenter.Evaluator where

import Carpenter.Base
import Carpenter.Parser
import Carpenter.Syntax
import Control.Monad.State
import Data.Map as M (Map, insert, lookup)
import Data.Text (Text)

type AST = Program

-- Helpers
-- Semantics
eval :: AST -> State Integer ()
eval (Program int) = do
  return ()
