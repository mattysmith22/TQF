{-# LANGUAGE RecordWildCards, TupleSections #-}
module TQF.Environment
    ( Environment(..)
    , CompiledModule(..)
    , EnvError
    , addUIdent
    , lookupUIdent
    , addLIdent
    , lookupLIdent
    , emptyEnv
    , importModuleToEnv
    ) where

import Data.Map (Map)
import TQF.AST
import TQF.AST.Annotated
import qualified Data.Map as Map
import Data.List.NonEmpty
import TQF.Type
import Data.Maybe (fromMaybe)
import SQF.Commands
import Data.String.Pretty
import Control.Arrow

data Environment = Environment
    { envUIdents :: Map UIdent (CanCollide Type)
    , envLIdents :: Map (ResolveableModule, VarName) (CanCollide ModLIdentDecl)
    }
    deriving (Show, Eq)

emptyEnv :: Environment
emptyEnv = Environment mempty mempty

data CanCollide a = NoCollision a
    | Collision
    deriving (Show, Eq)

data EnvError = EnvNotFound (Either (Annot UIdent) (Annot LIdent))
    | EnvCollision (Either (Annot UIdent) (Annot LIdent))
    deriving (Show, Eq)

instance Pretty EnvError where
    prettyPrint (EnvNotFound x) = "Not found: " ++ either prettyPrint prettyPrint x
    prettyPrint (EnvCollision x) = "Collision: " ++ either prettyPrint prettyPrint x

unpackLookupError :: Range -> Either UIdent LIdent -> Maybe (CanCollide a) -> Either EnvError a
unpackLookupError r ident Nothing = Left $ EnvNotFound $ Annot r +++ Annot r $ ident
unpackLookupError r ident (Just Collision) = Left $ EnvCollision $ Annot r +++ Annot r $ ident
unpackLookupError r ident (Just (NoCollision x)) = return x

lookupUIdent :: Range -> Environment -> UIdent -> Either EnvError Type
lookupUIdent r Environment{..} uident = unpackLookupError r (Left uident) (Map.lookup uident envUIdents)

addUIdent :: UIdent -> Type -> Environment -> Environment
addUIdent uident decl env = env { envUIdents = Map.insert uident (NoCollision decl) $ envUIdents env }

lookupLIdent :: Range -> Environment -> LIdent -> Either EnvError ResolvedLIdent
lookupLIdent r Environment{..} x@(LIdent modl (name:|rest)) = flip ResolvedLIdent rest <$> unpackLookupError r (Right x) (Map.lookup (modl,name) envLIdents)

addLIdent :: (ResolveableModule, VarName) -> ModLIdentDecl -> Environment -> Environment
addLIdent lident decl env = env { envLIdents = Map.insert lident (NoCollision decl) $ envLIdents env }

importModuleToEnv :: ResolveableModule -> CompiledModule -> Environment -> Environment
importModuleToEnv prefix CompiledModule{..} Environment{..} = Environment
    { envUIdents = Map.intersectionWith (const $ const Collision) envUIdents $ Map.mapKeys (UIdent prefix) modUIdents
    , envLIdents = Map.intersectionWith (const $ const Collision) envLIdents $ Map.mapKeys (prefix,) modLIdents
    }

data CompiledModule = CompiledModule
    { modUIdents :: Map TypeName Type
    , modLIdents :: Map VarName ModLIdentDecl
    }

instance Semigroup CompiledModule where
    l <> r = CompiledModule
        { modUIdents = modUIdents l <> modUIdents r
        , modLIdents = modLIdents l <> modLIdents r
        }
instance Monoid CompiledModule where
    mempty = CompiledModule mempty mempty
