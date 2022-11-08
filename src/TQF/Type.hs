{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module TQF.Type
    ( Within(..)
    , Type
    , BaseType
    , SimpleType(..)
    , ConstType
    
    , bottom
    , top
    , simpleType
    , constNumber
    , constString
    , constBool
    ) where

import Data.Functor.Identity ()
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Extra
import Test.QuickCheck

class Within a where
    isWithin :: a -> a -> Bool

data Type
    = Top
    | Options (Set BaseType)
    deriving (Show)

instance Eq Type where
    l == r = l `isWithin` r && r `isWithin` l
instance Within Type where
    _ `isWithin` Top = True
    Top `isWithin` _ = False
    (Options s) `isWithin` (Options l) = all (\x -> any (x `isWithin`) l) s

instance Arbitrary Type where
    arbitrary = frequency
        [ (1, return Top)
        , (19, arbitrary)
        ]

data BaseType
    = SimpleType SimpleType
    | ConstType ConstType
    deriving (Ord, Eq, Show)

instance Arbitrary BaseType where
    arbitrary = oneof
        [ SimpleType <$> elements [minBound..maxBound]
        , ConstType <$> arbitrary
        ]

instance Within BaseType where
    SimpleType s `isWithin` SimpleType l = s == l
    ConstType s `isWithin` ConstType l = s == l
    ConstType s `isWithin` SimpleType l = typeOfConst s == l
    _ `isWithin` _ = False


typeOfConst :: ConstType -> SimpleType
typeOfConst (ConstNumber _) = Number
typeOfConst (ConstString _) = String
typeOfConst (ConstBool _) = Bool

data ConstType = ConstNumber Double | ConstString String | ConstBool Bool
    deriving (Ord, Eq, Show)

instance Arbitrary ConstType where
    arbitrary = oneof
        [ ConstNumber <$> arbitrary
        , ConstString <$> arbitrary
        , ConstBool <$> arbitrary
        ]
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

constNumber :: Double -> Type
constNumber = Options . Set.singleton . ConstType . ConstNumber

constString :: String -> Type
constString = Options . Set.singleton . ConstType . ConstString

constBool :: Bool -> Type
constBool = Options . Set.singleton . ConstType . ConstBool
