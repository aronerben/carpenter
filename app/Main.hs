module Main
  ( main
  ) where

import Data.Text (Text)
import Data.Text.IO as T (readFile)

import Carpenter.Parser (parser)

main :: IO ()
main = do
  file <- T.readFile "examples/ex1.txt"
  case parser file of
    Left err -> putStr err
    Right ast -> print ast
