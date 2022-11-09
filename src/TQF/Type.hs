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
    , tuple
    , array
    , code
    ) where

import Data.Functor.Identity ()
import Data.Set (Set)
import Data.Zip
import qualified Data.Set as Set
import Data.List.Extra
import Test.QuickCheck

class Within a where
    isWithin :: a -> a -> Bool

data Type
    = Top
    | Options (Set BaseType)
    deriving (Show, Ord)

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
    | ArrayType Type
    | TupleType [Type]
    | CodeType [Type] Type
    deriving (Ord, Eq, Show)

instance Arbitrary BaseType where
    arbitrary = oneof
        [ SimpleType <$> elements [minBound..maxBound]
        , ConstType <$> arbitrary
        , ArrayType <$> arbitrary
        , TupleType <$> arbitrary
        ]

instance Within BaseType where
    SimpleType s `isWithin` SimpleType l = s == l
    SimpleType _ `isWithin` ConstType _ = False
    SimpleType _ `isWithin` ArrayType _ = False
    SimpleType _ `isWithin` TupleType _ = False
    SimpleType _ `isWithin` CodeType _ _ = False
    
    ConstType s `isWithin` SimpleType l = typeOfConst s == l
    ConstType s `isWithin` ConstType l = s == l
    ConstType _ `isWithin` ArrayType _ = False
    ConstType _ `isWithin` TupleType _ = False
    ConstType _ `isWithin` CodeType _ _ = False

    ArrayType _ `isWithin` SimpleType Array = True
    ArrayType _ `isWithin` SimpleType _ = False
    ArrayType _ `isWithin` ConstType _ = False
    ArrayType s `isWithin` ArrayType l = s `isWithin` l
    ArrayType _ `isWithin` TupleType _ = False
    ArrayType _ `isWithin` CodeType _ _ = False
    
    TupleType _ `isWithin` SimpleType Array = True
    TupleType _ `isWithin` SimpleType _ = False
    TupleType _ `isWithin` ConstType _ = False
        -- A tuple with one element is the same as an array of that type
    TupleType [s] `isWithin` ArrayType l = isWithin s l
    TupleType _ `isWithin` ArrayType _ = False
    TupleType s `isWithin` TupleType l = length s <= length l && and (zipWith isWithin s l)
    TupleType _ `isWithin` CodeType _ _ = False

    CodeType _ _ `isWithin` SimpleType Code = True
    CodeType _ _ `isWithin` SimpleType _ = False
    CodeType _ _ `isWithin` ConstType _ = False
    CodeType _ _ `isWithin` ArrayType _ = False
    CodeType _ _ `isWithin` TupleType _ = False
    CodeType sarg sret `isWithin` CodeType larg lret
        = all (uncurry $ flip isWithin) (zipPadded top (simpleType Nil) sarg larg) && sret `isWithin` lret
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
data SimpleType = Number | String | Bool | Array | Code | Nil
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

tuple :: [Type] -> Type
tuple = Options . Set.singleton . TupleType

array :: Type -> Type
array = Options . Set.singleton . ArrayType

code :: [Type] -> Type -> Type
code args ret = Options $ Set.singleton $ CodeType args ret