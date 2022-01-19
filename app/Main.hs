module Main where

import           Text.Pretty.Simple

import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           TQF.Lexer                     as Lexer
import           TQF.Parser                    as Parser

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    [filePath] -> do
      text <- readFile filePath
      pPrint $ Parser.parse $ Lexer.alexScanTokens text
    _ -> do
      putStrLn "Please enter file name to parse"
      exitWith (ExitFailure 1)
