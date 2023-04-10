{-# LANGUAGE RecordWildCards #-}
module TQF.Resolve.Env
    ( Environment(..)
    , CompiledModule(..)
    , EnvError(..)
    , addUIdent
    , lookupUIdent
    , addLIdent
    , lookupLIdent
    , emptyEnv
    , importModuleToEnv
    ) where

import           Control.Arrow
import           Data.List.NonEmpty
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Data.String.Pretty
import           SQF.Commands
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Type

data Environment = Environment
    { envUIdents :: Map UIdent (CanCollide GenericType)
    , envLIdents :: Map LIdent (CanCollide ModLIdentDecl)
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
    prettyPrint (EnvNotFound x)  = "Not found: " ++ either prettyPrint prettyPrint x
    prettyPrint (EnvCollision x) = "Collision: " ++ either prettyPrint prettyPrint x

unpackLookupError :: Range -> Either UIdent LIdent -> Maybe (CanCollide a) -> Either EnvError a
unpackLookupError r ident Nothing                = Left $ EnvNotFound $ Annot r +++ Annot r $ ident
unpackLookupError r ident (Just Collision)       = Left $ EnvCollision $ Annot r +++ Annot r $ ident
unpackLookupError r ident (Just (NoCollision x)) = return x

lookupUIdent :: Range -> Environment -> UIdent -> Either EnvError GenericType
lookupUIdent r Environment{..} uident = unpackLookupError r (Left uident) (Map.lookup uident envUIdents)

addUIdent :: UIdent -> GenericType -> Environment -> Environment
addUIdent uident decl env = env { envUIdents = Map.insert uident (NoCollision decl) $ envUIdents env }

lookupLIdent :: Range -> Environment -> LIdent -> Either EnvError ModLIdentDecl
lookupLIdent r Environment{..} x@(LIdent modl name) = unpackLookupError r (Right x) (Map.lookup x envLIdents)

addLIdent :: LIdent -> ModLIdentDecl -> Environment -> Environment
addLIdent lident decl env = env { envLIdents = Map.insert lident (NoCollision decl) $ envLIdents env }

importModuleToEnv :: ResolveableModule -> CompiledModule -> Environment -> Environment
importModuleToEnv prefix CompiledModule{..} Environment{..} = Environment
    { envUIdents = Map.unionWith (const $ const Collision) envUIdents $ Map.mapKeys (UIdent prefix) $ fmap NoCollision modUIdents
    , envLIdents = Map.unionWith (const $ const Collision) envLIdents $ Map.mapKeys (LIdent prefix) $ fmap NoCollision modLIdents
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
