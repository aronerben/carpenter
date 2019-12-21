module Tbel.Base
  ( Parser
  , Var(..)
  ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

-- General aliases
type Parser = Parsec Void Text

-- Needed so that Expr and Assignment have the same Var
newtype Var =
  Var String
  deriving (Show)
