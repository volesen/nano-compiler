module Ast where

type Name = String

newtype Literal
  = Number Integer
  deriving (Show)

data UnOp
  = Not
  deriving (Show)

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | NotEqual
  deriving (Eq, Show)

data Expr
  = Lit Literal
  | Id Name
  | UnOp UnOp Expr
  | BinOp BinOp Expr Expr
  | Call Name [Expr]
  deriving (Show)

data Stmt
  = Return Expr
  | Block [Stmt]
  | If Expr Stmt Stmt
  | Function Name [Name] Stmt
  | Var Name Expr
  | Assign Name Expr
  | While Expr Stmt
  | Expr Expr
  deriving (Show)

newtype Program = Program [Stmt]
  deriving (Show)