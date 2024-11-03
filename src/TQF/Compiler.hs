{-# LANGUAGE RecordWildCards #-}
module TQF.Compiler
    ( CompilerConfig(..)
    , CompilerEnv(..)
    , SQFExportInfo(..)
    , defCfg
    , defEnv
    , runCompiler
    ) where

import           Data.List
import           Data.Traversable
import qualified SQF.AST                    as SQF
import qualified SQF.FunctionConfig         as FunctionConfig
import           System.Directory
import           System.Directory.Recursive
import           System.FilePath
import qualified System.FilePath.Windows    as Windows
import           TQF
import qualified TQF.CodeGen                as CodeGen
import qualified TQF.CodeGen.Optimiser      as Optimiser
import           TQF.Control.Graph
import           TQF.Resolve.Env            (CompiledModule)
import           TQF.Types

data CompilerConfig = CompilerConfig
    { sqfExportDir    :: FilePath
    , pboPrefix       :: FilePath
    , tqfDir          :: FilePath
    , cfgFunctionPath :: FilePath
    }

defCfg :: CompilerConfig
defCfg = CompilerConfig
    { sqfExportDir = "sqf"
    , pboPrefix = ""
    , tqfDir = "tqf"
    , cfgFunctionPath = "TQFCfgFunctions.hpp"
    }

data CompileModuleResult = CompileModuleResult
    { resultSQFExport  :: SQFExportInfo
    , resultModuleName :: ResolveableModule
    , resultModule     :: Either String CompiledModule
    }

data CompilerEnv = CompilerEnv
    { loadConfig        :: IO CompilerConfig
    , findFilesWithExt  :: String -> FilePath -> IO [FilePath]
    , writeLog          :: String -> IO ()
    , readCompilerFile  :: FilePath -> IO String
    , writeCompilerFile :: FilePath -> String -> IO ()
    }

data SQFExportInfo = SQFExportInfo
    { exportFolder    :: FilePath
    , exportFolderPbo :: FilePath
    , exportModule    :: ResolveableModule
    , exportFunctions :: [(String, FilePath)]
    }

defEnv :: CompilerEnv
defEnv = CompilerEnv
    { loadConfig = return defCfg
    , findFilesWithExt = \ext -> fmap (filter ((==ext) . takeExtension)) . getFilesRecursive
    , writeLog = putStrLn
    , readCompilerFile = readFile
    , writeCompilerFile = \fp x -> createDirectoryIfMissing True (dropFileName fp) >> writeFile fp x
    }

data PathType = Standard | PBO

sqfFolderForModule :: CompilerConfig -> PathType -> ResolveableModule -> FilePath
sqfFolderForModule CompilerConfig{..} pathType mod = prefix `j` foldr1 j (unTypeName <$> mod)
    where
    prefix = case pathType of Standard -> sqfExportDir; PBO -> pboPrefix
    j = case pathType of Standard -> (</>); PBO -> (Windows.</>)

runCompiler :: CompilerEnv -> IO ()
runCompiler CompilerEnv{..} = do
    cfg <- loadConfig
    writeLog "Finding TQF Files"
    tqfFiles <- findFilesWithExt ".tqf" (tqfDir cfg)
    writeLog $ show (length tqfFiles) <> " files detected"

    let pathToModule' = pathToModule (tqfDir cfg)
    let moduleToPath' = moduleToPath (tqfDir cfg)

    compileGraph <- dependencyGraph (compileNode cfg moduleToPath')
    sqfExports <- mapM (requestNode compileGraph . pathToModule') tqfFiles >>= mapM (fmap resultSQFExport . readNode)
    writeFunctionsConfig sqfExports cfg
    clearRedundantSQFFiles sqfExports cfg
    writeLog "end"
    where
    compileNode
        :: CompilerConfig
        -> (ResolveableModule -> FilePath)
        -> GraphContext ResolveableModule CompileModuleResult
        -> ResolveableModule
        -> IO CompileModuleResult
    compileNode cfg moduleToFile graphCtx mod = do
        res <- compileModule moduleToFile
                (\_ dep -> resultModule
                    <$> (requestNodeCtx graphCtx dep >>= readNode)) mod
        let exportFolder = sqfFolderForModule cfg Standard mod
        let exportFolderPbo = sqfFolderForModule cfg PBO mod
        let codeGenResult
                = Optimiser.runOptimisations
                $ CodeGen.codeGen (either error id (typeCheckedModule res))
        exportFunctions <- forM (CodeGen.generatedFunctions codeGenResult) $ \(funName, funContent) -> do
            let scriptFp = exportFolder </> ("fn_"++unVarName funName) <.> "sqf"
            writeCompilerFile scriptFp (SQF.prettyPrint funContent)
            return (unVarName funName, scriptFp)
        let exportModule = mod
        return CompileModuleResult
                { resultSQFExport = SQFExportInfo{..}
                , resultModuleName = mod
                , resultModule = compiledModule res
                }

    clearRedundantSQFFiles :: [SQFExportInfo] -> CompilerConfig -> IO ()
    clearRedundantSQFFiles exports cfg = do
        let usedFilePaths = snd <$> concatMap exportFunctions exports
        toRemove <- filter (`notElem` usedFilePaths)<$> findFilesWithExt ".sqf" (sqfExportDir cfg)
        mapM_ removeFile toRemove

    writeFunctionsConfig :: [SQFExportInfo] -> CompilerConfig -> IO ()
    writeFunctionsConfig exports cfg = do
        let configEntries
                = (\SQFExportInfo{..} -> FunctionConfig.ModuleConfig
                    { modulePath = exportFolderPbo
                    , moduleTag = intercalate "_" $ unTypeName <$> exportModule
                    , moduleFunctions = fmap fst exportFunctions
                    })
                <$> filter (not . null . exportFunctions) exports
        writeCompilerFile (sqfExportDir cfg </> "TQFCfgFunctions.hpp") $ FunctionConfig.writeModuleConfigs configEntries ""
