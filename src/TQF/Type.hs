{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module TQF.Type
    ( Type
    , Type'
    , BaseType
    , SimpleType(..)
    , ConstType
    , GenericType(..)

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
    , simpleTypesOf
    , validateFuncCall
    , validateBinOp
    , validateUnOp
    , runInference
    ) where

import           Control.Monad         (unless)
import           Data.Foldable         (traverse_)
import           Data.Functor.Identity ()
import           Data.List             (intercalate)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust, isJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.String.Pretty
import           Data.Zip
import           Test.QuickCheck

class Within a where
    isWithin :: a -> a -> Bool

data Type' a
    = Top -- Any type matches this type
    | Options (Set (BaseType a)) -- A set of finite types which the type can be part of
    deriving (Show, Ord, Eq)

type Type = Type' String

data GenericType
    = GenericType
    { genTypeArgs    :: [String]
    , genTypeContent :: Type' String
    }
    deriving (Show, Eq)

instance Ord a => Within (Type' a) where
    _ `isWithin` Top                   = True
    Top `isWithin` _                   = False
    (Options s) `isWithin` (Options l) = all (\x -> any (x `isWithin`) l) s

instance Arbitrary a => Arbitrary (Type' a) where
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
instance Arbitrary a => Arbitrary (BaseType a) where
    arbitrary = oneof
        [ SimpleType <$> elements [minBound..maxBound]
        , ConstType <$> arbitrary
        , ArrayType <$> arbitrary
        , TupleType <$> arbitrary
        , RecordType <$> arbitrary
        , ExtraType <$> arbitrary
        ]

instance Ord a => Within (BaseType a) where
    SimpleType s `isWithin` SimpleType l = s == l
    SimpleType _ `isWithin` ConstType _ = False
    SimpleType _ `isWithin` ArrayType _ = False
    SimpleType _ `isWithin` TupleType _ = False
    SimpleType _ `isWithin` CodeType _ _ = False
    SimpleType _ `isWithin` RecordType _ = False

    ConstType s `isWithin` SimpleType l = typeOfConst s == l
    ConstType s `isWithin` ConstType l = s == l
    ConstType _ `isWithin` ArrayType _ = False
    ConstType _ `isWithin` TupleType _ = False
    ConstType _ `isWithin` CodeType _ _ = False
    ConstType _ `isWithin` RecordType _ = False

    ArrayType _ `isWithin` SimpleType Array = True
    ArrayType _ `isWithin` SimpleType _ = False
    ArrayType _ `isWithin` ConstType _ = False
    ArrayType s `isWithin` ArrayType l = s `isWithin` l
    ArrayType _ `isWithin` TupleType _ = False
    ArrayType _ `isWithin` CodeType _ _ = False
    ArrayType _ `isWithin` RecordType _ = False

    TupleType _ `isWithin` SimpleType Array = True
    TupleType _ `isWithin` SimpleType _ = False
    TupleType _ `isWithin` ConstType _ = False
        -- A tuple with one element is the same as an array of that type
    TupleType [s] `isWithin` ArrayType l = isWithin s l
    TupleType _ `isWithin` ArrayType _ = False
    TupleType s `isWithin` TupleType l = length s <= length l && and (zipWith isWithin s l)
    TupleType _ `isWithin` CodeType _ _ = False
    TupleType _ `isWithin` RecordType _ = False

    CodeType _ _ `isWithin` SimpleType Code = True
    CodeType _ _ `isWithin` SimpleType _ = False
    CodeType _ _ `isWithin` ConstType _ = False
    CodeType _ _ `isWithin` ArrayType _ = False
    CodeType _ _ `isWithin` TupleType _ = False
    CodeType sarg sret `isWithin` CodeType larg lret
        = all (uncurry $ flip isWithin) (zipPadded top (simpleType Nil) sarg larg) && sret `isWithin` lret
    CodeType _ _ `isWithin` RecordType _ = False

    RecordType _ `isWithin` SimpleType HashMap = True
    RecordType _ `isWithin` SimpleType _ = False
    RecordType _ `isWithin` ConstType _ = False
    RecordType _ `isWithin` ArrayType _ = False
    RecordType _ `isWithin` TupleType _ = False
    RecordType _ `isWithin` CodeType _ _ = False
    RecordType s `isWithin` RecordType l =  s `isWithin` l

    ExtraType s `isWithin` ExtraType l = s == l
    ExtraType _ `isWithin` _  = False
    _ `isWithin` ExtraType _ = False

instance Within a => Within (Map String a) where
    s `isWithin` l = Map.null (l `Map.difference` s) -- The subtype must have an implementation of every field
        && and (Map.intersectionWith isWithin s l) -- The subtypes implementation of each field must be a subtype itself

typeOfConst :: ConstType -> SimpleType
typeOfConst (ConstNumber _) = Number
typeOfConst (ConstString _) = String
typeOfConst (ConstBool _)   = Bool

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
simpleTypeToString String  = "string"
simpleTypeToString Number  = "num"
simpleTypeToString Array   = "array"
simpleTypeToString HashMap = "hashmap"
simpleTypeToString Bool    = "bool"
simpleTypeToString Code    = "code"
simpleTypeToString Nil     = "nil"

instance Ord a => Semigroup (Type' a) where
    Top <> _                   = Top
    _ <> Top                   = Top
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
intersect (Options ls) (Options rs)
    = Options $ Set.map fromJust $ Set.filter isJust $ Set.map (uncurry intersectBase) $ Set.cartesianProduct ls rs
    where
    intersectBase :: BaseType String -> BaseType String -> Maybe (BaseType String)
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

    intersectSimpleConst :: SimpleType -> ConstType -> Maybe (BaseType String)
    intersectSimpleConst s c = if typeOfConst c == s then Just $ ConstType c else Nothing

    intersectSimpleArray :: SimpleType -> Type -> Maybe (BaseType String)
    intersectSimpleArray Array t = Just $ ArrayType t
    intersectSimpleArray _ _     = Nothing

    intersectSimpleTuple :: SimpleType -> [Type] -> Maybe (BaseType String)
    intersectSimpleTuple Array ts = Just $ TupleType ts
    intersectSimpleTuple _ _      = Nothing

    intersectSimpleCode :: SimpleType -> ([Type], Type) -> Maybe (BaseType String)
    intersectSimpleCode Code (ca, cr) = Just $ CodeType ca cr
    intersectSimpleCode _ _           = Nothing

    intersectSimpleRecord :: SimpleType -> Map String Type -> Maybe (BaseType String)
    intersectSimpleRecord HashMap x = Just $ RecordType x
    intersectSimpleRecord _ _       = Nothing

    intersectArrayTuple :: Type -> [Type] -> Maybe (BaseType String)
    intersectArrayTuple lt [rt] = fmap (TupleType . pure) $ stripIfBottom $ intersect lt rt
    intersectArrayTuple _ _     = Nothing

    intersectTupleTuple :: [Type] -> [Type] -> Maybe (BaseType String)
    intersectTupleTuple lTypes rTypes = Just $ TupleType $ takeWhile (not . isBottom) $ zipWith intersect lTypes rTypes

    intersectCodeCode :: ([Type], Type) -> ([Type], Type) -> Maybe (BaseType String)
    intersectCodeCode (la, lr) (ra, rr)
        | isValid = Just $ CodeType args ret
        | otherwise = Nothing
        where
            isValid = not (isBottom ret) && not (any isBottom args)
            ret = lr `intersect` rr
            args = uncurry intersect <$> zipPadded top top la ra

    intersectRecordRecord :: Map String Type -> Map String Type -> Maybe (BaseType String)
    intersectRecordRecord lfields rfields
        | any isBottom middle = Nothing
        | otherwise = Just $ RecordType $ lOnly <> rOnly <> middle
        where
            lOnly = lfields `Map.difference` rfields
            rOnly = rfields `Map.difference` lfields
            middle = Map.intersectionWith intersect lfields rfields

    stripIfBottom :: Type -> Maybe Type
    stripIfBottom x = if isBottom x then Nothing else Just x

    isBottom :: Type -> Bool
    isBottom Top          = False
    isBottom (Options os) = Set.null os

simpleTypesOf :: Type' String -> Type' String
simpleTypesOf Top = Top
simpleTypesOf (Options xs) = Options $ Set.fromList [SimpleType sx | x<-Set.toList xs, sx<-simpleTypeOf x]
    where
        simpleTypeOf (SimpleType x) = [x]
        simpleTypeOf (ConstType x)  = [typeOfConst x]
        simpleTypeOf (ArrayType _ ) = [Array]
        simpleTypeOf (TupleType _ ) = [Array]
        simpleTypeOf (CodeType _ _) = [Code]
        simpleTypeOf (RecordType _) = [HashMap]
        simpleTypeOf (ExtraType _)  =[]

resolveType :: (Ord b, Applicative m) => (a -> m (Type' b)) -> Type' a -> m (Type' b)
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
        lookupFieldBaseType :: BaseType String -> Maybe Type
        lookupFieldBaseType (RecordType x) = Map.lookup field x
        lookupFieldBaseType _              = Nothing

validateFuncCall :: Type -> [Type] -> Either (Type, Type) Type
validateFuncCall Top argsIn = Left (Top, code argsIn Top)
validateFuncCall (Options xs) argsIn
    = fmap mconcat $ traverse validateFuncCallBaseType $ Set.toList xs
    where
        validateFuncCallBaseType :: BaseType String -> Either (Type, Type) Type
        validateFuncCallBaseType (CodeType args ret) = do
            traverse_ (\x -> unless (uncurry isWithin x) $ Left x) $ zipPadded (simpleType Nil) top argsIn args
            return ret
        validateFuncCallBaseType notCode = Left (Options $ Set.singleton notCode, code argsIn Top)

validateBinOp :: [(SimpleType,SimpleType,SimpleType)] -> Type -> Type -> Either (Type,Type) Type
validateBinOp _ Top r = Left (Top,r)
validateBinOp _ l Top = Left (l,Top)
validateBinOp validOps (Options l) (Options r) = fmap mconcat $ mapM (uncurry matches) $ Set.toList $ Set.cartesianProduct l r
    where
        matches :: BaseType String -> BaseType String -> Either (Type, Type) Type
        matches l' r' = foldl1 eitherOf $ fmap (isValidOp l' r') validOps

        isValidOp :: BaseType String -> BaseType String -> (SimpleType, SimpleType, SimpleType) -> Either (Type,Type) Type
        isValidOp l' r' (lExp, rExp, ret)= let
            lt = Options $ Set.singleton l'
            rt = Options $ Set.singleton r'
            in if lt `isWithin` simpleType lExp && rt `isWithin` simpleType rExp then
                Right $ simpleType ret
            else
                Left (lt, rt)

        eitherOf :: Either a b -> Either a b -> Either a b
        eitherOf (Left _) (Right x) = Right x
        eitherOf (Left x) (Left _)  = Left x
        eitherOf (Right x) _        = Right x

validateUnOp :: [(SimpleType,SimpleType)] -> Type -> Either Type Type
validateUnOp _ Top = Left Top
validateUnOp validOps (Options os) = fmap mconcat $ mapM matches $ Set.toList os
    where
        matches :: BaseType String -> Either Type Type
        matches x' = foldl1 eitherOf $ fmap (isValidOp x') validOps

        isValidOp :: BaseType String -> (SimpleType, SimpleType) -> Either Type Type
        isValidOp x' (xExp, ret) = let
            xt = Options $ Set.singleton x'
            in if xt `isWithin` simpleType xExp then
                Right $ simpleType ret
            else
                Left xt

        eitherOf :: Either a b -> Either a b -> Either a b
        eitherOf (Left _) (Right x) = Right x
        eitherOf (Left x) (Left _)  = Left x
        eitherOf (Right x) _        = Right x

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
            showBaseTypePretty (TupleType ts) = "(" ++ intercalate "," (fmap prettyPrint ts) ++ ")"
            showBaseTypePretty (CodeType args ret) = "(" ++ intercalate "," (fmap prettyPrint args) ++ ") -> " ++ prettyPrint ret
            showBaseTypePretty (RecordType fields) = "{" ++ intercalate ", " ((\(k,v) -> k ++ ": " ++ prettyPrint v) <$> Map.toList fields) ++ "}"
            showBaseTypePretty (ExtraType x) = x

runInference :: (Ord a, Ord b) => Type' a -> Type' b -> Map a (Type' b)
runInference l r = Map.fromListWith (<>) $ runInference' l r

runInference' :: Type' a -> Type' b -> [(a, Type' b)]
runInference' Top _ = []
runInference' _ Top = []
runInference' (Options ls) (Options rs) = concatMap (uncurry runInferenceBase) $ Set.cartesianProduct ls rs
    where
        runInferenceBase :: BaseType a -> BaseType b -> [(a, Type' b)]
        runInferenceBase (SimpleType _) _ = []
        runInferenceBase (ConstType _) _ = []
        runInferenceBase (ArrayType lt) (ArrayType rt) = runInference' lt rt
        runInferenceBase (ArrayType _) _ = []
        runInferenceBase (TupleType ls) (TupleType rs) = concat $ zipWith runInference' ls rs
        runInferenceBase (TupleType _) _ = []
        runInferenceBase (CodeType largs lret) (CodeType rargs rret) = concat (zipWith runInference' largs rargs) ++ runInference' lret rret
        runInferenceBase (CodeType _ _) _ = []
        runInferenceBase (RecordType lrec) (RecordType rrec) = concatMap (uncurry runInference') $ Map.intersectionWith (,) lrec rrec
        runInferenceBase (RecordType _) _ = []
        runInferenceBase (ExtraType a) x = [(a, Options $ Set.singleton x)]
