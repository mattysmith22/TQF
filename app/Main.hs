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
import qualified TQF.CodeGen                   as CodeGen
import qualified SQF.AST                       as SQF
import           Data.String.Pretty

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      text <- readFile filePath
      
      let parsed = either error id $ Lexer.runAlex text Parser.parse
      resolveResult <- resolveModule (const $ error "Cannot parse modules") parsed
      case resolveResult of
        (Left err) -> putStrLn $ prettyPrint err
        (Right (resolved,compiledModule)) -> case TypeCheck.typeCheck resolved of
            (Left err) -> putStrLn $ prettyPrint err
            (Right ()) -> do
                putStrLn "Passed TypeCheck"
                putStrLn $ SQF.prettyPrint $ CodeGen.codeGen resolved
    _ -> do
      putStrLn "Please enter file name to parse"
      exitWith (ExitFailure 1)
