module Compile
  ( compile,
  )
where

import CodeGen (emitProgram, runEmitter)
import Parser (parse)

compile :: String -> Either String String
compile src = do
  ast <- parse src
  return $ runEmitter $ emitProgram ast
