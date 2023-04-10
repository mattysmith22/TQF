{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module TQF
  ( pathForModule
  , compileModule
  , CompileResult(..)
  ) where

import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.List.Extra            (intercalate)
import           Data.String.Pretty
import           System.FilePath
import           TQF.AST
import           TQF.Lexer                  as Lexer
import           TQF.Parser                 as Parser
import           TQF.Resolve                as Resolve
import           TQF.Resolve.Env
import           TQF.TypeCheck              as TypeCheck

data CompileResult = CompileResult
  { lexedModule       :: Either String [Token]
  , parsedModule      :: Either String (Module Parsed)
  , resolvedModule    :: Either String (Module Resolved)
  , typeCheckedModule :: Either String (Module Resolved)
  , compiledModule    :: Either String CompiledModule
  }

class ModuleIdentifier a where
  toModFilePath :: (ResolveableModule -> FilePath) -> a -> FilePath
  toModIdentifier :: a -> ResolveableModule
instance ModuleIdentifier FilePath where
  toModFilePath _ = id
  toModIdentifier = const [TypeName "<LIVE>"]
instance ModuleIdentifier ResolveableModule where
  toModFilePath x = x
  toModIdentifier = id

compileModule
    :: ModuleIdentifier path
    => (ResolveableModule -> FilePath)
    -> [ResolveableModule]
    -> path
    -> IO CompileResult
compileModule pathForModule evalPath moduleName
  | moduleId `elem` evalPath = error $ "Cyclic import: " ++ intercalate ":" (prettyPrint <$> takeWhile (/=moduleId) evalPath)
  | otherwise = do
    text <- readFile $ toModFilePath pathForModule moduleName
    let lexedModule = Lexer.runAlex text Lexer.listTokens
    let parsedModule = Lexer.runAlex text Parser.parse
    resolveResult <- runExceptT $ (ExceptT . runResolver) =<< ExceptT (return parsedModule)
    let resolvedModule = fst <$> resolveResult
    let compiledModule' = snd <$> resolveResult
    let isTypeChecked = resolveResult >>= (first prettyPrint . TypeCheck.typeCheck . fst)
    let typeCheckedModule = isTypeChecked *> resolvedModule
    let compiledModule = typeCheckedModule *> compiledModule'
    return CompileResult{..}
  where
    moduleId = toModIdentifier moduleName

    runResolver ::  Module Parsed -> IO (Either String (Module Resolved, CompiledModule))
    runResolver
      = fmap (first prettyPrint =<<)
      . runExceptT
      . resolveModule (ExceptT . fmap compiledModule . compileModule pathForModule (moduleId:evalPath))

pathForModule :: ResolveableModule -> FilePath
pathForModule = (<.> ".tqf") . joinPath . fmap unTypeName
