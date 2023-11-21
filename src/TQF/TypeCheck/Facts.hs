{-# LANGUAGE RecordWildCards #-}
module TQF.TypeCheck.Facts
    ( IdentKey
    , Facts
    , toIdentKey
    , addIdentTypeFact
    , resolveIdentType
    ) where

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           TQF.AST
import           TQF.Resolve (Resolved)
import           TQF.Type    (GenericType (..), Type')

data IdentKey
    = DeclKey ResolveableModule VarName Int
    deriving (Ord, Eq, Show)

toIdentKey :: Ident Resolved -> Maybe IdentKey
toIdentKey (Ident idnt []) = Just $ toIdentKey' idnt
    where
        toIdentKey' ModLIdentDecl{..} = DeclKey lIdentModule lIdentName lIdentId
toIdentKey (Ident _ _) = Nothing

newtype Facts = Facts
    { identTypes :: Map IdentKey (Type' String)
    }

instance Semigroup Facts where
    l <> r = Facts
        { identTypes = Map.intersectionWith (<>) (identTypes l) (identTypes r)
        }

instance Monoid Facts where
    mempty = Facts mempty

addIdentTypeFact :: Ident Resolved -> Type' String -> Facts -> Facts
addIdentTypeFact ident factType facts
    | Just identKey <- toIdentKey ident = facts { identTypes = Map.insert identKey factType $ identTypes facts}
    | otherwise = facts

resolveIdentType :: Facts -> Ident Resolved -> Ident Resolved
resolveIdentType facts ident
    | Just identKey <- toIdentKey ident
    , (GenericType [] _) <- lIdentType $ identName ident
    , Just inferredType <- Map.lookup identKey (identTypes facts)
    = ident {identName = (identName ident :: ModLIdentDecl) { lIdentType = GenericType [] inferredType}}
    | otherwise = ident
