module Ast where

type Name = String

data Expr
  = Number Integer
  | Id Name
  | Not Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Call Name [Expr]
  | Return Expr
  | Block [Expr]
  | If Expr Expr Expr
  | Function Name [Name] Expr
  | Var Name Expr
  | Assign Name Expr
  | While Expr Expr
  deriving (Eq, Show)

newtype Program = Program [Expr]
  deriving (Eq, Show)