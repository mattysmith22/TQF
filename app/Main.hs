module Main where

import Text.Pretty.Simple

import TQF.Lexer as Lexer
import TQF.Parser as Parser
import System.Environment (getArgs)
import System.Exit ( exitWith, ExitCode(ExitFailure) )

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