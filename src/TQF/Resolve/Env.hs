{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE RecordWildCards        #-}
module TQF.Resolve.Env
    ( Environment(..)
    , CompiledModule(..)
    , EnvError(..)
    , envLookup
    , envAdd
    , emptyEnv
    , envCount
    , envNewScope
    , importModuleToEnv
    ) where

import           Control.Applicative
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.String.Pretty
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve.Types
import           TQF.Type

data Environment = Environment
    { envUIdents :: Map UIdent (CanCollide GenericType)
    , envLIdents :: Map LIdent (CanCollide ModLIdentDecl)
    , envParent  :: Maybe Environment
    }
    deriving (Show, Eq)

emptyEnv :: Environment
emptyEnv = Environment mempty mempty Nothing

data CanCollide a = NoCollision a
    | Collision
    deriving (Show, Eq)

data EnvError
    = EnvNotFound (Annot String)
    | EnvCollision (Annot String)
    | EnvInvalidLvalue (Expr Resolved)
    deriving (Show, Eq)

instance Pretty EnvError where
    prettyPrint (EnvNotFound x)      = "Not found: " ++ prettyPrint x
    prettyPrint (EnvCollision x)     = "Collision: " ++ prettyPrint x
    prettyPrint (EnvInvalidLvalue x) = "Invalid lvalue at " ++ prettyPrint (pos x)

class Ord ident => EnvLookup ident value | ident -> value where
    envLookupMap :: Environment -> (Map ident (CanCollide value), Map ident (CanCollide value) -> Environment)

instance EnvLookup UIdent GenericType where
    envLookupMap env = (envUIdents env, \x -> env {envUIdents = x})
instance EnvLookup LIdent ModLIdentDecl where
    envLookupMap env = (envLIdents env, \x -> env {envLIdents = x})

unpackLookupError :: Pretty ident => Range -> ident -> Maybe (CanCollide a) -> Either EnvError a
unpackLookupError r ident Nothing            = Left $ EnvNotFound $ Annot r $ prettyPrint ident
unpackLookupError r ident (Just Collision)   = Left $ EnvCollision $ Annot r $ prettyPrint ident
unpackLookupError _ _ (Just (NoCollision x)) = return x

recLookup :: (Environment -> Maybe a) -> Environment -> Maybe a
recLookup lookupF env = lookupF env <|> (recLookup lookupF =<< envParent env)

recCount :: (Environment -> Bool) -> Environment -> Int
recCount f env = (if f env then 1 else 0) + maybe 0 (recCount f) (envParent env)

envLookup :: (Pretty ident, EnvLookup ident value) => Range -> Environment -> ident -> Either EnvError value
envLookup r env ident = unpackLookupError r ident $ recLookup (Map.lookup ident . fst . envLookupMap) env

envAdd :: (Pretty ident, EnvLookup ident value) => Annot ident -> value -> Environment -> Either EnvError Environment
envAdd ident value env
    | Map.member (unAnnot ident) curMap = Left $ EnvCollision $ fmap prettyPrint ident
    | otherwise = return $ setter $ Map.insert (unAnnot ident) (NoCollision value) curMap
    where
        (curMap, setter) = envLookupMap env

envCount :: EnvLookup ident value => ident -> Environment -> Int
envCount ident = recCount (Map.member ident . fst . envLookupMap)

envNewScope :: Environment -> Environment
envNewScope parent = emptyEnv {envParent = Just parent}

importModuleToEnv :: ResolveableModule -> CompiledModule -> Environment -> Environment
importModuleToEnv prefix CompiledModule{..} Environment{..} = Environment
    { envUIdents = Map.unionWith (const $ const Collision) envUIdents $ Map.mapKeys (UIdent prefix) $ fmap NoCollision modUIdents
    , envLIdents = Map.unionWith (const $ const Collision) envLIdents $ Map.mapKeys (LIdent prefix) $ fmap NoCollision modLIdents
    , envParent = envParent
    }

data CompiledModule = CompiledModule
    { modUIdents :: Map TypeName GenericType
    , modLIdents :: Map VarName ModLIdentDecl
    }

instance Semigroup CompiledModule where
    (<>) :: CompiledModule -> CompiledModule -> CompiledModule
    l <> r = CompiledModule
        { modUIdents = modUIdents l <> modUIdents r
        , modLIdents = modLIdents l <> modLIdents r
        }
instance Monoid CompiledModule where
    mempty = CompiledModule mempty mempty
