module TQF where

import TQF.AST
import TQF.Resolve.Env
import           TQF.Lexer                     as Lexer
import           TQF.Parser                    as Parser
import           TQF.Resolve                   as Resolve
import           TQF.TypeCheck                 as TypeCheck
import           Data.List.Extra (nubOrd, intercalate, splitOn)
import           Data.String.Pretty
import           System.FilePath

compileModule
    :: (ResolveableModule -> FilePath)
    -> [ResolveableModule]
    -> Either ResolveableModule FilePath
    -> IO (Module Resolved, CompiledModule)
compileModule pathForModule evalPath moduleName
  | moduleName' `elem` evalPath = error $ "Cyclic import: " ++ intercalate ":" (prettyPrint <$> takeWhile (/=moduleName') evalPath)
  | otherwise = do
    putStrLn $ "Compiling " ++ prettyPrint moduleName'
    text <- readFile $ either pathForModule id moduleName
    let parsed = either error id $ Lexer.runAlex text Parser.parse
    resolveResult <- resolveModule (fmap snd . compileModule pathForModule (moduleName':evalPath) . Left) parsed
    case resolveResult of
      (Left err) -> error $ prettyPrint err
      (Right (resolved,compiledModule)) -> case TypeCheck.typeCheck resolved of
          (Left err) -> error $ prettyPrint err
          (Right ()) -> do
              return (resolved, compiledModule)
  where
    moduleName' = case moduleName of
      (Left x) -> x
      (Right y) -> [TypeName "<LIVE>"]

pathForModule :: ResolveableModule -> FilePath
pathForModule = (<.> ".tqf") . joinPath . fmap unTypeName