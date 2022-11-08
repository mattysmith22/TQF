{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
module TQF.Type
    ( Within(..)
    , Type(..)
    , BaseType(..)
    , SimpleType(..)
    
    , bottom
    , top
    , simpleType
    )where

import Data.Functor.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Extra

class Within a where
    isWithin :: a -> a -> Bool

data Type
    = Top
    | Options (Set BaseType)
    deriving (Ord, Eq, Show)

instance Within Type where
    _ `isWithin` Top = True
    Top `isWithin` _ = False
    (Options s) `isWithin` (Options l) = all (\x -> any (x `isWithin`) l) s

newtype BaseType
    = SimpleType SimpleType
    deriving (Ord, Eq, Show)

instance Within BaseType where
    SimpleType s `isWithin` SimpleType l = s == l

data SimpleType = Number | String | Bool
    deriving (Ord, Eq, Show, Bounded, Enum)

instance Semigroup Type where
    Top <> _ = Top
    _ <> Top = Top
    (Options l) <> (Options r) = Options (l <> r)

instance Monoid Type where
    mempty = Options mempty

bottom :: Type
bottom = mempty

top :: Type
top = Top

simpleType :: SimpleType -> Type
simpleType = Options . Set.singleton . SimpleType
