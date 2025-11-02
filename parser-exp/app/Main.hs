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
    ["happy", "stmt"] -> print $ stmt toks
    ["happy", "expr"] -> print $ expr toks
    ["happy", "prog"] -> print $ prog toks
    ["mega", "expr"] -> putStrLn "Megaparsec Expr parser not implemented yet"
    ["mega", "stmt"] -> putStrLn "Megaparsec Stmt parser not implemented yet"
    ["mega", "prog"] -> putStrLn "Megaparsec Prog parser not implemented yet"
    _ -> do
      putStrLn "Usage: parser-exp-exe FRAMEWORK PARSER"
      putStrLn "\tFRAMEWORK: happy, mega"
      putStrLn "\tPARSER: prog, stmt, expr"
