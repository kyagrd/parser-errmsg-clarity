module Main (main) where

import Lexer
import Parser

main :: IO ()
main = do
  input <- getContents
  let toks = lexer input
  print $ parser toks
