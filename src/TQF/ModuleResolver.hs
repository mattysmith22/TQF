{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
module TQF.ModuleResolver where

import           Control.Monad
import Control.Monad.State.Lazy
    ( MonadState(put, get), MonadTrans(lift), StateT(runStateT) )
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           TQF.AST

data Namespace = Namespace
  { path       :: ResolveableModule
  , lowerIdent :: Map.Map VarName [Declaration]
  , upperIdent :: Map.Map TypeName (Either Namespace TypeName)
  }
  deriving (Eq, Show)

addLocalVar :: VarName -> Type -> Namespace -> Namespace
addLocalVar ident typ ns = ns {
  lowerIdent = Map.insert ident val $ lowerIdent ns 
}
  where
    val = [VariableDecl typ ident]

findLIdent :: Namespace -> Var -> Maybe [Declaration]
findLIdent Namespace{..} (Var [] varName) = Map.lookup varName lowerIdent
findLIdent n@Namespace{..} (Var (i:is) varName) = Map.lookup i upperIdent >>=
  \case
    (Left ns) -> findLIdent n (Var is varName)
    (Right ns) -> Nothing

data ResolverError = NamespaceTypeClash Type
    | UIdentClash ResolveableModule TypeName
    deriving (Eq, Show)

type NamespaceTransformation a = StateT Namespace (Either ResolverError) a

runNamespaceTransformation
  :: NamespaceTransformation a -> Namespace -> Either ResolverError Namespace
runNamespaceTransformation transform = fmap snd . runStateT transform

runNamespaceTransformation'
  :: NamespaceTransformation a -> Namespace -> Either ResolverError (a, Namespace)
runNamespaceTransformation' = runStateT

initialNamespace :: Namespace
initialNamespace = Namespace [] Map.empty Map.empty

onChildNamespace :: TypeName -> NamespaceTransformation a -> NamespaceTransformation a
onChildNamespace ident transform = do
  state <- get
  child <- case Map.lookup ident (upperIdent state) of
    Nothing                -> return $ Namespace (path state ++ [ident]) Map.empty Map.empty
    Just (Left  namespace) -> return namespace
    Just (Right _        ) -> lift $ Left $ NamespaceTypeClash $ Type (path state) ident
  (ret, child') <- lift $ runNamespaceTransformation' transform child
  put $ state { upperIdent = Map.insert ident (Left child') (upperIdent state) }
  return ret

insertLookup :: Ord k => k -> a -> Map.Map k a -> (Maybe a, Map.Map k a)
insertLookup = Map.insertLookupWithKey (\_ a _ -> a)

currentModule :: NamespaceTransformation ResolveableModule
currentModule = path <$> get

addLowerIdent :: VarName -> Declaration -> NamespaceTransformation ()
addLowerIdent ident val = do
  state <- get
  let state' = Map.insertWith (<>) ident [val] (lowerIdent state)
  put state { lowerIdent = state' }
  return ()

addUpperIdent :: TypeName -> Either Namespace TypeName -> NamespaceTransformation ()
addUpperIdent ident val = do
  state <- get
  let (mVal, state') = insertLookup ident val (upperIdent state)
  curPath <- currentModule
  when (isJust mVal) $ lift $ Left $ UIdentClash curPath ident
  put state { upperIdent = state' }
  return ()

writeDeclarationsAt :: ResolveableModule -> Module -> NamespaceTransformation ()
writeDeclarationsAt (ident : modules) ast =
  onChildNamespace ident (writeDeclarationsAt modules ast)
writeDeclarationsAt [] ast = writeDeclarations ast
 where
  writeDeclarations :: Module -> NamespaceTransformation ()
  writeDeclarations Module { moduleDeclarations = declarations } =
    mapM_ writeDeclaration declarations

  writeDeclaration :: Declaration -> NamespaceTransformation ()
  writeDeclaration f@FunctionDecl { functionName = name } = addLowerIdent name f
  writeDeclaration v@VariableDecl { variableName }        = addLowerIdent variableName v

addModule :: ImportStatement -> Module -> NamespaceTransformation ()
addModule (ImportStatement name isQualified mNewName) ast = do
  writeDeclarationsAt (fromMaybe name mNewName) ast
  unless isQualified $ writeDeclarationsAt [] ast
