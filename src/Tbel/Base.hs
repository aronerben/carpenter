module Tbel.Base
  ( Parser
  , Header
  , Identifier(..)
  ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

-- General aliases
type Parser = Parsec Void Text

newtype Identifier =
  Identifier String
  deriving (Show, Eq)

type Header = Identifier
