{-# LANGUAGE RecordWildCards #-}
module Main where

import           Text.Pretty.Simple

import           Control.Monad          (forM, forM_, when)
import           Data.Char              (isUpper)
import           Data.List.Extra        (intercalate, nubOrd, splitOn)
import           Data.Maybe             (fromJust, fromMaybe)
import           Options.Applicative
import qualified SQF.AST                as SQF
import           Safe                   (headMay)
import           System.Directory.Extra (createDirectoryIfMissing)
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (ExitFailure), exitWith)
import           System.FilePath        (joinPath, takeDirectory, takeExtension, (<.>))
import qualified System.FilePath.Find   as Find
import           TQF
import           TQF.AST
import qualified TQF.CodeGen            as CodeGen
import           TQF.CodeGen.Optimiser  as Optimiser
import           TQF.Resolve.Env        (CompiledModule)

data CompileArgs = CompileArgs
  { modulesToCompile :: [String]
  , printParsed      :: Bool
  , printResolved    :: Bool
  , printLexed       :: Bool
  , outDir           :: FilePath
  }

argParser :: ParserInfo CompileArgs
argParser = info (args <**> helper) mempty
  where
    args = CompileArgs
      <$> some (strArgument (help "Modules to compile"))
      <*> switch (long "print-parsed")
      <*> switch (long "print-resolved")
      <*> switch (long "print-lexed")
      <*> strOption (help "Output directory" <> value "out" <> long "output")

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe fp txt
  = createDirectoryIfMissing True (takeDirectory fp)
  >> writeFile fp txt

main :: IO ()
main = do
  CompileArgs{..} <- execParser argParser
  forM_ modulesToCompile $ \moduleName -> do
      CompileResult{..} <- compileModule pathForModule [] $ fromMaybe (error $ moduleName ++ " is not a valid module name") (splitModule moduleName)

      when (printLexed || printParsed || printResolved) $ putStrLn $ "Intermediate for " ++ moduleName
      when printLexed $ putStr "Lexed:" >> forM_ (either error id lexedModule) print
      when printParsed $ putStrLn "Parsed:" >> pPrint (either error id parsedModule)
      when printResolved $ putStrLn "Resolved" >> pPrint (either error id resolvedModule)

      putStrLn $ SQF.prettyPrint $ optimiseCommandCall <$> CodeGen.codeGen (either error id typeCheckedModule)

splitModule :: String -> Maybe ResolveableModule
splitModule = mapM readTypeName . splitOn "."
  where
    readTypeName :: String -> Maybe TypeName
    readTypeName "" = Nothing
    readTypeName x@(c:_)
      | isUpper c = Just $ TypeName x
      | otherwise = Nothing
