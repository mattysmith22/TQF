{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
module TQF.Resolve.Env
    ( Environment(..)
    , CompiledModule(..)
    , EnvError(..)
    , envLookup
    , envAdd
    , emptyEnv
    , envNewScope
    , importModuleToEnv
    ) where

import           Control.Applicative
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.String.Pretty
import           TQF.AST
import           TQF.AST.Annotated
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

envLookup :: (Pretty ident, EnvLookup ident value) => Range -> Environment -> ident -> Either EnvError value
envLookup r env ident = unpackLookupError r ident $ recLookup (Map.lookup ident . fst . envLookupMap) env

envAdd :: EnvLookup ident value => ident -> value -> Environment -> Environment
envAdd ident value env = setter $ Map.insertWith (const $ const Collision) ident (NoCollision value) curMap
    where
        (curMap, setter) = envLookupMap env

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
    l <> r = CompiledModule
        { modUIdents = modUIdents l <> modUIdents r
        , modLIdents = modLIdents l <> modLIdents r
        }
instance Monoid CompiledModule where
    mempty = CompiledModule mempty mempty
