module MyLib where

import CodeGen (emitProgram, runEmitter)
import Parser (parse)
import System.Environment

compile :: String -> Either String String
compile src = do
  ast <- parse src
  return $ runEmitter $ emitProgram ast

-- * Entry point


main :: IO ()
main = do
  -- Get first command line argument
  path <- head <$> getArgs

  -- Get second command line argument
  dest <- head . tail <$> getArgs

  -- Read file contents
  src <- readFile path

  -- Compile
  case compile src of
    Left err -> putStrLn err
    Right assembly -> writeFile dest assembly