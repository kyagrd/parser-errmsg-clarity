-- src/Types.hs
module Types where

data Expr
  = Plus Expr Expr
  | Times Expr Expr
  | Var String
  | Num Int
  | If Expr Expr Expr
  deriving (Show, Eq)