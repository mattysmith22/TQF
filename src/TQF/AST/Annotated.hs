{-# LANGUAGE InstanceSigs #-}
module TQF.AST.Annotated where

import Test.QuickCheck (Arbitrary(..))

data Pos = Pos
    { lines :: Int
    , cols :: Int
    }
    deriving (Show, Eq, Ord)

data Range = Range Pos Pos
    | NoPlace -- ^ The item has no place in the text which can be referenced
    deriving (Show, Eq)

instance Semigroup Range where
    (Range l1 l2) <> (Range r1 r2) = Range (l1 `min` r1) (l2 `max` r2)
    l@(Range _ _) <> NoPlace = l
    NoPlace <> r@(Range _ _) = r
    NoPlace <> NoPlace = NoPlace
instance Monoid Range where
    mempty = NoPlace

data Annot a = Annot
    { pos :: Range
    , unAnnot :: a
    }

noPlace :: a -> Annot a
noPlace = Annot NoPlace

-- Annot is designed to make no difference to structure and is only debugging information.
-- Ranges in annotations will neither be shown nor affect equivalence checks
instance Show a => Show (Annot a) where
    show (Annot _ x) = show x
instance Eq a => Eq (Annot a) where
    (Annot _ l) == (Annot _ r) = l == r
instance Ord a => Ord (Annot a) where
    (Annot _ l) `compare` (Annot _ r) = l `compare` r
instance Semigroup a => Semigroup (Annot a) where
    (Annot rl l) <> (Annot rr r) = Annot (rl <> rr) (l <> r)
instance Monoid a => Monoid (Annot a) where
    mempty = Annot NoPlace mempty
instance Arbitrary a => Arbitrary (Annot a) where
    arbitrary = Annot NoPlace <$> arbitrary

instance Functor Annot where
    fmap f (Annot p x) = Annot p (f x)
instance Applicative Annot where
    (Annot rf f) <*> (Annot rx x) = Annot (rf <> rx) (f x)
    pure = noPlace
instance Monad Annot where
    (Annot rx x) >>= f = let (Annot ry y) = f x
        in Annot (rx <> ry) y

instance Foldable Annot where
    foldr :: (a -> b -> b) -> b -> Annot a -> b
    foldr f acc (Annot _ x) = f x acc

instance Traversable Annot where
    traverse f (Annot p x) = Annot p <$> f x


dispRange :: Range -> Maybe String
dispRange (Range (Pos sl sc) (Pos el ec)) = Just $ show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
dispRange NoPlace = Nothing