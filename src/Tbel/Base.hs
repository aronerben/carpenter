module Tbel.Base
  ( Parser
  , Identifier(..)
  , Headers(..)
  ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

-- General aliases
type Parser = Parsec Void Text

newtype Identifier =
  Identifier String
  deriving (Show, Eq)

newtype Headers =
  Headers [String]
  deriving (Show, Eq)
