{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module TQF.Type
    ( Type
    , Type'
    , BaseType
    , SimpleType(..)
    , ConstType

    , intersect
    , isWithin

    , simpleTypeToString
    
    , bottom
    , top
    , simpleType
    , constNumber
    , constString
    , constBool
    , tuple
    , array
    , code
    , record
    , extra
    
    , resolveType
    , lookupField
    , validateFuncCall
    , validateBinOp
    , validateUnOp
    ) where

import Data.Functor.Identity ()
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Zip
import qualified Data.Set as Set
import Test.QuickCheck
import Control.Arrow
import Data.Foldable (traverse_, asum)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Control.Monad (unless)
import Data.Either (isRight, fromLeft, fromRight)
import SQF.Commands
import Data.String.Pretty

class Within a where
    isWithin' :: a -> a -> Bool

data Type' a
    = Top -- Any type matches this type
    | Options (Set (BaseType a)) -- A set of finite types which the type can be part of
    deriving (Show, Ord, Eq)

type Type = Type' ()

isWithin :: Type -> Type -> Bool
isWithin = isWithin'

instance Within Type where
    _ `isWithin'` Top = True
    Top `isWithin'` _ = False
    (Options s) `isWithin'` (Options l) = all (\x -> any (x `isWithin'`) l) s

instance Arbitrary Type where
    arbitrary = frequency
        [ (1, return Top)
        , (19, arbitrary)
        ]

data BaseType a
    -- | Arma simple types - Number, String, Object as well as untyped variants of
    -- other structures (HashMap, Array, Code)
    = SimpleType SimpleType 
    -- | Constant value
    | ConstType ConstType
    -- | Typed array (element of subtype is known)
    | ArrayType (Type' a)
    -- | Typed tuple (array with finite elements, different types)
    | TupleType [Type' a]
    -- | Typed code (input type, output types are known)
    -- TODO: allow for passing in of variables from external env in type system
    | CodeType [Type' a] (Type' a)
    -- | Typed record (a hashmap whose fields are known)
    | RecordType (Map String (Type' a))
    | ExtraType a
    deriving (Ord, Eq, Show)
instance Arbitrary (BaseType ()) where
    arbitrary = oneof
        [ SimpleType <$> elements [minBound..maxBound]
        , ConstType <$> arbitrary
        , ArrayType <$> arbitrary
        , TupleType <$> arbitrary
        , RecordType <$> arbitrary
        , ExtraType <$> arbitrary
        ]

instance Within (BaseType ()) where
    SimpleType s `isWithin'` SimpleType l = s == l
    SimpleType _ `isWithin'` ConstType _ = False
    SimpleType _ `isWithin'` ArrayType _ = False
    SimpleType _ `isWithin'` TupleType _ = False
    SimpleType _ `isWithin'` CodeType _ _ = False
    SimpleType _ `isWithin'` RecordType _ = False
    
    ConstType s `isWithin'` SimpleType l = typeOfConst s == l
    ConstType s `isWithin'` ConstType l = s == l
    ConstType _ `isWithin'` ArrayType _ = False
    ConstType _ `isWithin'` TupleType _ = False
    ConstType _ `isWithin'` CodeType _ _ = False
    ConstType _ `isWithin'` RecordType _ = False

    ArrayType _ `isWithin'` SimpleType Array = True
    ArrayType _ `isWithin'` SimpleType _ = False
    ArrayType _ `isWithin'` ConstType _ = False
    ArrayType s `isWithin'` ArrayType l = s `isWithin'` l
    ArrayType _ `isWithin'` TupleType _ = False
    ArrayType _ `isWithin'` CodeType _ _ = False
    ArrayType _ `isWithin'` RecordType _ = False
    
    TupleType _ `isWithin'` SimpleType Array = True
    TupleType _ `isWithin'` SimpleType _ = False
    TupleType _ `isWithin'` ConstType _ = False
        -- A tuple with one element is the same as an array of that type
    TupleType [s] `isWithin'` ArrayType l = isWithin s l
    TupleType _ `isWithin'` ArrayType _ = False
    TupleType s `isWithin'` TupleType l = length s <= length l && and (zipWith isWithin s l)
    TupleType _ `isWithin'` CodeType _ _ = False
    TupleType _ `isWithin'` RecordType _ = False

    CodeType _ _ `isWithin'` SimpleType Code = True
    CodeType _ _ `isWithin'` SimpleType _ = False
    CodeType _ _ `isWithin'` ConstType _ = False
    CodeType _ _ `isWithin'` ArrayType _ = False
    CodeType _ _ `isWithin'` TupleType _ = False
    CodeType sarg sret `isWithin'` CodeType larg lret
        = all (uncurry $ flip isWithin) (zipPadded top (simpleType Nil) sarg larg) && sret `isWithin'` lret
    CodeType _ _ `isWithin'` RecordType _ = False

    RecordType _ `isWithin'` SimpleType HashMap = True
    RecordType _ `isWithin'` SimpleType _ = False
    RecordType _ `isWithin'` ConstType _ = False
    RecordType _ `isWithin'` ArrayType _ = False
    RecordType _ `isWithin'` TupleType _ = False
    RecordType _ `isWithin'` CodeType _ _ = False
    RecordType s `isWithin'` RecordType l =  s `isWithin'` l

    ExtraType () `isWithin'` _ = True
    _ `isWithin'` ExtraType () = False

instance Within a => Within (Map String a) where
    s `isWithin'` l = Map.null (l `Map.difference` s) -- The subtype must have an implementation of every field
        && and (Map.intersectionWith isWithin' s l) -- The subtypes implementation of each field must be a subtype itself

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
data SimpleType = Number | String | Bool | Array | Code | Nil | HashMap
    deriving (Ord, Eq, Show, Bounded, Enum)

simpleTypeToString :: SimpleType -> String
simpleTypeToString String = "string"
simpleTypeToString Number = "num"
simpleTypeToString Array = "array"
simpleTypeToString HashMap = "hashmap"
simpleTypeToString Bool = "bool"
simpleTypeToString Code = "code"
simpleTypeToString Nil = "nil"

instance Ord a => Semigroup (Type' a) where
    Top <> _ = Top
    _ <> Top = Top
    (Options l) <> (Options r) = Options (l <> r)

instance Ord a => Monoid (Type' a) where
    mempty = Options mempty

bottom :: Ord a => Type' a
bottom = mempty

top :: Ord a => Type' a
top = Top

simpleType :: SimpleType -> Type' a
simpleType = Options . Set.singleton . SimpleType

constNumber :: Double -> Type' a
constNumber = Options . Set.singleton . ConstType . ConstNumber

constString :: String -> Type' a
constString = Options . Set.singleton . ConstType . ConstString

constBool :: Bool -> Type' a
constBool = Options . Set.singleton . ConstType . ConstBool

tuple :: [Type' a] -> Type' a
tuple = Options . Set.singleton . TupleType

array :: Type' a -> Type' a
array = Options . Set.singleton . ArrayType

code :: [Type' a] -> Type' a -> Type' a
code args ret = Options $ Set.singleton $ CodeType args ret

record :: Map String (Type' a) -> Type' a
record = Options . Set.singleton . RecordType

extra :: a -> Type' a
extra = Options . Set.singleton . ExtraType

intersect :: Type -> Type -> Type
intersect Top x = x
intersect x Top = x
intersect (Options l) (Options r)
    = Options $ Set.map fromJust $ Set.filter isJust $ Set.map (uncurry intersectBase) $ Set.cartesianProduct l r
    where
    intersectBase :: BaseType () -> BaseType () -> Maybe (BaseType ())
    SimpleType l `intersectBase` SimpleType r = if l == r then Just $ SimpleType l else Nothing
    SimpleType l `intersectBase` ConstType r = intersectSimpleConst l r
    SimpleType l `intersectBase` ArrayType r = intersectSimpleArray l r
    SimpleType l `intersectBase` TupleType r = intersectSimpleTuple l r
    SimpleType l `intersectBase` CodeType ra rr = intersectSimpleCode l (ra, rr)
    SimpleType l `intersectBase` RecordType r = intersectSimpleRecord l r

    ConstType l `intersectBase` SimpleType r = intersectSimpleConst r l
    ConstType l `intersectBase` ConstType r = if l == r then Just $ ConstType l else Nothing
    ConstType _ `intersectBase` ArrayType _ = Nothing
    ConstType _ `intersectBase` TupleType _ = Nothing
    ConstType _ `intersectBase` CodeType _ _ = Nothing
    ConstType _ `intersectBase` RecordType _ = Nothing

    ArrayType l `intersectBase` SimpleType r = intersectSimpleArray r l
    ArrayType _ `intersectBase` ConstType _ = Nothing
    ArrayType l `intersectBase` ArrayType r = ArrayType <$> stripIfBottom (l `intersect` r)
    ArrayType l `intersectBase` TupleType r = intersectArrayTuple l r
    ArrayType _ `intersectBase` CodeType _ _ = Nothing
    ArrayType _ `intersectBase` RecordType _ = Nothing

    TupleType l `intersectBase` SimpleType r = intersectSimpleTuple r l
    TupleType _ `intersectBase` ConstType _ = Nothing
        -- A tuple with one element is the same as an array of that type
    TupleType l `intersectBase` ArrayType r = intersectArrayTuple r l
    TupleType l `intersectBase` TupleType r = intersectTupleTuple l r
    TupleType _ `intersectBase` CodeType _ _ = Nothing
    TupleType _ `intersectBase` RecordType _ = Nothing

    CodeType la lr `intersectBase` SimpleType r = intersectSimpleCode r (la, lr)
    CodeType _ _ `intersectBase` ConstType _ = Nothing
    CodeType _ _ `intersectBase` ArrayType _ = Nothing
    CodeType _ _ `intersectBase` TupleType _ = Nothing
    CodeType larg lret `intersectBase` CodeType rarg rret
        = intersectCodeCode (larg, lret) (rarg, rret)
    CodeType _ _ `intersectBase` RecordType _ = Nothing

    RecordType l `intersectBase` SimpleType r = intersectSimpleRecord r l
    RecordType _ `intersectBase` ConstType _ = Nothing
    RecordType _ `intersectBase` ArrayType _ = Nothing
    RecordType _ `intersectBase` TupleType _ = Nothing
    RecordType _ `intersectBase` CodeType _ _ = Nothing
    RecordType l `intersectBase` RecordType r =  intersectRecordRecord l r

    ExtraType _ `intersectBase` _ = Nothing
    _ `intersectBase` ExtraType _ = Nothing

    intersectSimpleConst :: SimpleType -> ConstType -> Maybe (BaseType ())
    intersectSimpleConst s c = if typeOfConst c == s then Just $ ConstType c else Nothing

    intersectSimpleArray :: SimpleType -> Type -> Maybe (BaseType ())
    intersectSimpleArray Array t = Just $ ArrayType t
    intersectSimpleArray _ _ = Nothing

    intersectSimpleTuple :: SimpleType -> [Type] -> Maybe (BaseType ())
    intersectSimpleTuple Array ts = Just $ TupleType ts
    intersectSimpleTuple _ _ = Nothing

    intersectSimpleCode :: SimpleType -> ([Type], Type) -> Maybe (BaseType ())
    intersectSimpleCode Code (ca, cr) = Just $ CodeType ca cr
    intersectSimpleCode _ _ = Nothing

    intersectSimpleRecord :: SimpleType -> Map String Type -> Maybe (BaseType ())
    intersectSimpleRecord HashMap x = Just $ RecordType x
    intersectSimpleRecord _ _ = Nothing

    intersectArrayTuple :: Type -> [Type] -> Maybe (BaseType ())
    intersectArrayTuple l [r] = fmap (TupleType . pure) $ stripIfBottom $ intersect l r
    intersectArrayTuple _ _ = Nothing

    intersectTupleTuple :: [Type] -> [Type] -> Maybe (BaseType ())
    intersectTupleTuple l r = Just $ TupleType $ takeWhile (not . isBottom) $ zipWith intersect l r

    intersectCodeCode :: ([Type], Type) -> ([Type], Type) -> Maybe (BaseType ())
    intersectCodeCode (la, lr) (ra, rr)
        | isValid = Just $ CodeType args ret
        | otherwise = Nothing
        where
            isValid = not (isBottom ret) && not (any isBottom args)
            ret = lr `intersect` rr
            args = uncurry intersect <$> zipPadded top top la ra

    intersectRecordRecord :: Map String Type -> Map String Type -> Maybe (BaseType ())
    intersectRecordRecord l r 
        | any isBottom middle = Nothing
        | otherwise = Just $ RecordType $ lOnly <> rOnly <> middle
        where
            lOnly = l `Map.difference` r
            rOnly = r `Map.difference` l
            middle = Map.intersectionWith intersect l r

    stripIfBottom :: Type -> Maybe Type
    stripIfBottom x = if isBottom x then Nothing else Just x

    isBottom :: Type -> Bool
    isBottom Top = False
    isBottom (Options os) = Set.null os

resolveType :: Applicative m => (a -> m Type) -> Type' a -> m Type
resolveType _ Top = pure Top
resolveType lookupF (Options xs) = mconcat <$> traverse resolveBaseType (Set.toList xs)
    where
        resolveBaseType (SimpleType x) = pure $ Options $ Set.singleton $ SimpleType x
        resolveBaseType (ConstType x) = pure $ Options $ Set.singleton $ ConstType x
        resolveBaseType (ArrayType x) = Options . Set.singleton . ArrayType <$> resolveType lookupF x
        resolveBaseType (TupleType x) = Options . Set.singleton . TupleType <$> traverse (resolveType lookupF) x
        resolveBaseType (CodeType x y) = (\a b -> Options $ Set.singleton $ CodeType a b) <$> traverse (resolveType lookupF) x <*> resolveType lookupF y
        resolveBaseType (RecordType x) = Options . Set.singleton . RecordType <$> traverse (resolveType lookupF) x
        resolveBaseType (ExtraType x) = lookupF x

lookupField :: String -> Type -> Maybe Type
lookupField _ Top = Nothing
lookupField field (Options xs)
    = fmap mconcat $ traverse lookupFieldBaseType $ Set.toList xs
    where
        lookupFieldBaseType :: BaseType () -> Maybe Type
        lookupFieldBaseType (RecordType x) = Map.lookup field x
        lookupFieldBaseType _ = Nothing

validateFuncCall :: Type -> [Type] -> Either (Type, Type) Type
validateFuncCall Top argsIn = Left (Top, code argsIn Top)
validateFuncCall (Options xs) argsIn
    = fmap mconcat $ traverse validateFuncCallBaseType $ Set.toList xs
    where
        validateFuncCallBaseType :: BaseType () -> Either (Type, Type) Type
        validateFuncCallBaseType (CodeType args ret) = do
            traverse_ (\x -> unless (uncurry isWithin x) $ Left x) $ zipPadded (simpleType Nil) top argsIn args
            return ret
        validateFuncCallBaseType notCode = Left (Options $ Set.singleton notCode, code argsIn Top)

validateBinOp :: [(SimpleType,SimpleType,SimpleType)] -> Type -> Type -> Either (Type,Type) Type
validateBinOp validOps Top r = Left (Top,r)
validateBinOp validOps l Top = Left (l,Top)
validateBinOp validOps (Options l) (Options r) = fmap mconcat $ mapM (uncurry matches) $ Set.toList $ Set.cartesianProduct l r
    where
        matches :: BaseType () -> BaseType () -> Either (Type, Type) Type
        matches l' r' = foldl1 eitherOf $ fmap (isValidOp l' r') validOps

        isValidOp :: BaseType () -> BaseType () -> (SimpleType, SimpleType, SimpleType) -> Either (Type,Type) Type
        isValidOp l' r' (lExp, rExp, ret)= let
            lt = Options $ Set.singleton l'
            rt = Options $ Set.singleton r'
            in if lt `isWithin` simpleType lExp && rt `isWithin` simpleType rExp then
                Right $ simpleType ret
            else
                Left (lt, rt)

        eitherOf :: Either a b -> Either a b -> Either a b
        eitherOf (Left _) (Right x) = Right x
        eitherOf (Left x) (Left _) = Left x
        eitherOf (Right x) _ = Right x

validateUnOp :: [(SimpleType,SimpleType)] -> Type -> Either Type Type
validateUnOp validOps Top = Left Top
validateUnOp validOps (Options x) = fmap mconcat $ mapM matches $ Set.toList x
    where
        matches :: BaseType () -> Either Type Type
        matches x' = foldl1 eitherOf $ fmap (isValidOp x') validOps

        isValidOp :: BaseType () -> (SimpleType, SimpleType) -> Either Type Type
        isValidOp x' (xExp, ret) = let
            xt = Options $ Set.singleton x'
            in if xt `isWithin` simpleType xExp then
                Right $ simpleType ret
            else
                Left xt

        eitherOf :: Either a b -> Either a b -> Either a b
        eitherOf (Left _) (Right x) = Right x
        eitherOf (Left x) (Left _) = Left x
        eitherOf (Right x) _ = Right x

instance Pretty Type where
    prettyPrint Top = "top"
    prettyPrint (Options xs)
        | Set.null xs = "bottom"
        | otherwise = intercalate "|" $ showBaseTypePretty <$> Set.toList xs
        where
            showBaseTypePretty (SimpleType x) = simpleTypeToString x
            showBaseTypePretty (ConstType (ConstString x)) = "\""++x++"\""
            showBaseTypePretty (ConstType (ConstNumber x)) = show x
            showBaseTypePretty (ConstType (ConstBool True)) = "true"
            showBaseTypePretty (ConstType (ConstBool False)) = "false"
            showBaseTypePretty (ArrayType x) = "[" ++ prettyPrint x ++ "]"
            showBaseTypePretty (TupleType [x]) = "'(" ++ prettyPrint x ++ ")"
            showBaseTypePretty (TupleType xs) = "(" ++ intercalate "," (fmap prettyPrint xs) ++ ")"
            showBaseTypePretty (CodeType args ret) = "(" ++ intercalate "," (fmap prettyPrint args) ++ ") -> " ++ prettyPrint ret
            showBaseTypePretty (RecordType fields) = "{" ++ intercalate ", " ((\(k,v) -> k ++ ": " ++ prettyPrint v) <$> Map.toList fields) ++ "}"
            showBaseTypePretty (ExtraType ()) = ""