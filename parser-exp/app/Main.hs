module Main (main) where

import Lexer
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  let toks = lexer input
  case args of
    ["stmt"] -> print $ stmt toks
    ["expr"] -> print $ expr toks
    _ -> putStrLn "Usage: parser-exp-exe stmt or parser-exp-exe expr"
