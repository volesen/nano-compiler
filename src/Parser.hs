{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser where

import Ast
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (id)

type Parser = Parsec Void String

-- * Lexer

-- | Space consumer
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- * Parsing helpers

integer :: Parser Integer
integer = lexeme L.decimal

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

-- * Expressions

{-
args        ::= ('(' expression (',' expression)* ')')?
call        ::= ID '(' args ')'
atom        ::= call / ID / NUMBER / '(' expression ')'
unary       ::= NOT? atom
product     ::= unary (('*' / '/') unary)*
sum         ::= product (('+' / '-') product)*
comparison  ::= sum (('==' / '!=') sum)*
expression  ::= comparison
-}

ident :: Parser String
ident = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')

  let name = first : rest

  if name `elem` keywords
    then fail $ "keyword " ++ name ++ " cannot be an identifier"
    else return name
  where
    keywords =
      [ "return",
        "if",
        "else",
        "while",
        "var",
        "function"
      ]

number :: Parser Literal
number = Number <$> integer

numberLit :: Parser Expr
numberLit = Lit <$> number

id :: Parser Expr
id = Id <$> ident

call :: Parser Expr
call = Call <$> ident <*> parens args
  where
    args = expr `sepBy` symbol ","

term :: Parser Expr
term =
  try call
    <|> id
    <|> numberLit
    <|> parens expr

expr :: Parser Expr
expr = makeExprParser term table
  where
    table =
      [ [ Prefix (UnOp Not <$ symbol "!")
        ],
        [ InfixL (BinOp Multiply <$ symbol "*"),
          InfixL (BinOp Divide <$ symbol "/")
        ],
        [ InfixL (BinOp Add <$ symbol "+"),
          InfixL (BinOp Subtract <$ symbol "-")
        ],
        [ InfixL (BinOp Equal <$ symbol "=="),
          InfixL (BinOp NotEqual <$ symbol "!=")
        ]
      ]

-- * Statements

{-
returnStatement    ::= "return" expression ";"
expressionStatement ::= expression ";"
ifStatement        ::= "if" "(" expression ")" statement "else" statement
whileStatement     ::= "while" "(" expression ")" statement
varStatement       ::= "var" ID "=" expression ";"
assignmentStatement::= ID "=" expression ";"
blockStatement     ::= "{" statement* "}"
parameters         ::= (ID ("," ID)*)?
functionStatement  ::= "function" ID "(" parameters ")" blockStatement
statement          ::= returnStatement
                        | ifStatement
                        | whileStatement
                        | varStatement
                        | assignmentStatement
                        | blockStatement
                        | functionStatement
                        | expressionStatement
-}

returnStmt :: Parser Stmt
returnStmt = do
  symbol "return"
  value <- expr
  symbol ";"

  return (Return value)

exprStmt :: Parser Stmt
exprStmt = Expr <$> expr <* symbol ";"

ifStmt :: Parser Stmt
ifStmt = do
  symbol "if"
  condition <- parens expr
  consequent <- stmt
  symbol "else"
  alternative <- stmt

  return (If condition consequent alternative)

whileStmt :: Parser Stmt
whileStmt = do
  symbol "while"
  condition <- parens expr
  body <- stmt

  return (While condition body)

varStmt :: Parser Stmt
varStmt = do
  symbol "var"
  name <- ident
  symbol "="
  value <- expr
  symbol ";"

  return (Var name value)

assignStmt :: Parser Stmt
assignStmt = do
  name <- ident
  symbol "="
  value <- expr
  symbol ";"

  return (Assign name value)

blockStmt :: Parser Stmt
blockStmt = Block <$> braces (many stmt)

functionStmt :: Parser Stmt
functionStmt = do
  symbol "function"
  name <- ident
  parameters <- params
  body <- blockStmt

  return (Function name parameters body)
  where
    params = parens (ident `sepBy` symbol ",")

stmt :: Parser Stmt
stmt =
  returnStmt
    <|> ifStmt
    <|> whileStmt
    <|> varStmt
    <|> functionStmt
    <|> blockStmt
    <|> try assignStmt
    <|> exprStmt

-- * Program

program = Program <$> many stmt <* eof

-- * Entry point

parse :: String -> Either String Program
parse input = case runParser program "" input of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast