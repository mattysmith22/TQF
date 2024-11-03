{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module TQF
  ( pathToModule
  , moduleToPath
  , compileModule
  , trivialResolver
  , CompileResult(..)
  , ResolveableModule
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

trivialResolver
    :: (ResolveableModule -> FilePath)
    -> ResolveableModule
    -> ResolveableModule
    -> IO (Either String CompiledModule)
trivialResolver = trivialResolver' []
    where
    trivialResolver'
        :: [ResolveableModule]
        -> (ResolveableModule -> FilePath)
        -> ResolveableModule
        -> ResolveableModule
        -> IO (Either String CompiledModule)
    trivialResolver' path pathResolver curModule nextModule
        | nextModule `elem` path
            = error $ "Cyclic import: " ++
                intercalate ":" (prettyPrint <$> takeWhile (/=nextModule) path)
        | otherwise
            = compiledModule
            <$> compileModule pathResolver (trivialResolver' (curModule:path) pathResolver) nextModule

compileModule
    :: (ResolveableModule -> FilePath)
    -> (ResolveableModule -> ResolveableModule -> IO (Either String CompiledModule))
    -> ResolveableModule
    -> IO CompileResult
compileModule pathResolver resolver moduleName = do
    text <- readFile $ pathResolver moduleName
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
    runResolver ::  Module Parsed -> IO (Either String (Module Resolved, CompiledModule))
    runResolver
      = fmap (first prettyPrint =<<)
      . runExceptT
      . resolveModule (ExceptT . resolver moduleName)

pathToModule :: FilePath -> FilePath -> ResolveableModule
pathToModule tqfDir = fmap TypeName . splitDirectories . dropExtension . makeRelative tqfDir

moduleToPath :: FilePath -> ResolveableModule -> FilePath
moduleToPath tqfDir = (tqfDir </>) . (<.> "tqf") . joinPath . fmap unTypeName
