{-# LANGUAGE RecordWildCards #-}
module TQF.Compiler
    ( CompilerConfig(..)
    , CompilerEnv(..)
    , defCfg
    , defEnv
    , runCompiler
    ) where

import qualified SQF.AST                    as SQF
import           System.Directory.Recursive
import           System.FilePath
import           TQF
import qualified TQF.CodeGen                as CodeGen
import qualified TQF.CodeGen.Optimiser      as Optimiser
import           TQF.Control.Graph

data CompilerConfig = CompilerConfig
    { addonDir        :: FilePath
    , tqfDir          :: FilePath
    , functionDir     :: FilePath
    , cfgFunctionPath :: FilePath
    }

defCfg :: CompilerConfig
defCfg = CompilerConfig
    { addonDir = "Addons"
    , tqfDir = "tqf"
    , functionDir = "functions"
    , cfgFunctionPath = "TQFCfgFunctions.hpp"
    }

type CompileModuleResult = CompileResult

data CompilerEnv = CompilerEnv
    { loadConfig   :: IO CompilerConfig
    , findTQFFiles :: FilePath -> IO [FilePath]
    , writeLog     :: String -> IO ()
    , readTQFFile  :: FilePath -> IO String
    , writeTQFFile :: FilePath -> String -> IO ()
    }

defEnv :: CompilerEnv
defEnv = CompilerEnv
    { loadConfig = return defCfg
    , findTQFFiles = fmap (filter ((==".tqf") . takeExtension)) . getFilesRecursive
    , writeLog = putStrLn
    , readTQFFile = readFile
    , writeTQFFile = writeFile
    }

runCompiler :: CompilerEnv -> IO ()
runCompiler env@CompilerEnv{..} = do
    cfg <- loadConfig
    writeLog "Finding TQF Files"
    tqfFiles <- findTQFFiles (tqfDir cfg)
    writeLog $ show (length tqfFiles) <> " files detected"

    let pathToModule' = pathToModule (tqfDir cfg)
    let moduleToPath' = moduleToPath (tqfDir cfg)

    compileGraph <- dependencyGraph (compileNode env cfg moduleToPath')
    mapM (requestNode compileGraph . pathToModule') tqfFiles >>= mapM_ readNode
    writeLog "end"
    where
    compileNode
        :: CompilerEnv
        -> CompilerConfig
        -> (ResolveableModule -> FilePath)
        -> GraphContext ResolveableModule CompileModuleResult
        -> ResolveableModule
        -> IO CompileModuleResult
    compileNode _env _cfg moduleToFile graphCtx mod = do
        res <- compileModule moduleToFile (\_ dep -> compiledModule <$> (requestNodeCtx graphCtx dep >>= readNode)) mod
        writeLog $ SQF.prettyPrint $ CodeGen.toScript $ Optimiser.runOptimisations $ CodeGen.codeGen (either error id (typeCheckedModule res))
        return res
