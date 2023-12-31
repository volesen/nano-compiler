module Compile
  ( compile,
  )
where

import CodeGen (emitProgram, execCodeGen)
import Parser (parseProgram)

compile :: String -> Either String String
compile src = do
  ast <- parseProgram src
  asm <- execCodeGen (emitProgram ast)
  return asm