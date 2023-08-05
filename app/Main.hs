module Main where

import Compile (compile)
import Options.Applicative

data Opts = Opts
  { srcFile :: FilePath,
    outFile :: Maybe FilePath
  }
  deriving (Show)

main :: IO ()
main = do
  Opts srcFile outFile <- execParser optsParser

  src <- readFile srcFile

  case compile src of
    Left err -> putStrLn err
    Right assembly ->
      case outFile of
        Nothing -> putStrLn assembly
        Just out -> writeFile out assembly
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> opts)
        ( fullDesc
            <> progDesc "Compile a source file"
            <> header "my-compiler"
        )

    opts :: Parser Opts
    opts =
      Opts
        <$> argument str (metavar "SRC")
        <*> optional (argument str (metavar "OUT"))