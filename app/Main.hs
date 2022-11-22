{-# LANGUAGE RecordWildCards #-}
module Main where

import           Text.Pretty.Simple

import           System.Environment             ( getArgs )
import           System.Exit                    ( ExitCode(ExitFailure)
                                                , exitWith
                                                )
import           TQF.AST
import           TQF.Lexer                     as Lexer
import           TQF.Parser                    as Parser
import           TQF.Resolve                   as Resolve
import           TQF.TypeCheck                 as TypeCheck
import qualified TQF.CodeGen                   as CodeGen
import           TQF.Resolve.Env (CompiledModule)
import qualified SQF.AST                       as SQF
import           Data.List.Extra (nubOrd, intercalate, splitOn)
import           Data.String.Pretty
import           Options.Applicative
import           Data.Char (isUpper)
import qualified System.FilePath.Find as Find
import           Control.Monad(forM_, forM)
import           Data.Maybe (fromMaybe)
import           System.FilePath (takeExtension, takeDirectory, joinPath, (<.>))
import System.Directory.Extra (createDirectoryIfMissing)
import Safe (headMay)

data CompileArgs = CompileArgs
  { modulesToCompile :: [String]
  , outDir :: FilePath
  }

argParser :: ParserInfo CompileArgs
argParser = info (args <**> helper) mempty
  where
    args = CompileArgs
      <$> some (strArgument (help "Modules to compile"))
      <*> strOption (help "Output directory" <> value "out" <> long "output")

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe fp txt
  = createDirectoryIfMissing True (takeDirectory fp)
  >> writeFile fp txt

compileModule :: [ResolveableModule] -> ResolveableModule -> IO (Module Resolved, CompiledModule)
compileModule evalPath moduleName
  | moduleName `elem` evalPath = error $ "Cyclic import: " ++ intercalate ":" (prettyPrint <$> takeWhile (/=moduleName) evalPath)
  | otherwise = do
    putStrLn $ "Compiling " ++ prettyPrint moduleName
    text <- readFile (pathForModule moduleName)
    let parsed = either error id $ Lexer.runAlex text Parser.parse
    resolveResult <- resolveModule (fmap snd . compileModule (moduleName:evalPath)) parsed
    case resolveResult of
      (Left err) -> error $ prettyPrint err
      (Right (resolved,compiledModule)) -> case TypeCheck.typeCheck resolved of
          (Left err) -> error $ prettyPrint err
          (Right ()) -> do
              return (resolved, compiledModule)

main :: IO ()
main = do
  CompileArgs{..} <- execParser argParser
  forM_ modulesToCompile $ \moduleName -> do
      (resolved, _) <- compileModule [] (fromMaybe (error $ moduleName ++ " is not a valid module name") $ splitModule moduleName)
      putStrLn $ SQF.prettyPrint $ CodeGen.codeGen resolved

splitModule :: String -> Maybe ResolveableModule
splitModule = mapM readTypeName . splitOn "."
  where
    readTypeName :: String -> Maybe TypeName
    readTypeName "" = Nothing
    readTypeName x@(c:_)
      | isUpper c = Just $ TypeName x
      | otherwise = Nothing

pathForModule :: ResolveableModule -> FilePath
pathForModule = (<.> ".tqf") . joinPath . fmap unTypeName