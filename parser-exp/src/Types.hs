-- src/Types.hs
module Types where

data Expr
  = Plus Expr Expr
  | Times Expr Expr
  | Var String
  | Num Int
  | If Expr Expr Expr
  | FunCall String [Expr]
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | VarDecl String
  | FunDecl String [String] [Stmt] Expr
  | Block [Stmt]
  | IfStmt Expr Stmt Stmt
  deriving (Show, Eq)