module Main where

import           Text.Pretty.Simple

import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           TQF.Lexer                     as Lexer
import           TQF.Parser                    as Parser
import           TQF.Resolve                   as Resolve
import           TQF.TypeCheck                 as TypeCheck

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    [filePath] -> do
      text <- readFile filePath
      let lexed = Lexer.alexScanTokens text
      let parsed = either error id $ Parser.parse lexed
      resolved <- either (error . show) id <$> resolveModule (const $ error "Cannot parse modules") parsed
      let typeChecked = either (error . show) id $ TypeCheck.typeCheck resolved
      pPrint resolved
    _ -> do
      putStrLn "Please enter file name to parse"
      exitWith (ExitFailure 1)
