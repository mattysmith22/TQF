{-# LANGUAGE FlexibleInstances #-}
module TQF.TypeSpec
    ( spec
    ) where
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           TQF.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

implies :: Bool -> Bool -> Bool
implies l r = not l || r

withinEachOther :: Type -> Type -> Bool
withinEachOther l r = l `isWithin'` r && r `isWithin'` l

isWithin' :: Type -> Type -> Bool
isWithin' = isWithin

spec :: Spec
spec = parallel $ modifyMaxSuccess (const 10000) $ do
    describe "monoid" $ do
        it "Anything merged with Top is Top" $ do
            (top <> top) `shouldBe` (top :: Type)
            (top <> bottom) `shouldBe` (top :: Type)
            (top <> bottom) `shouldBe` (top :: Type)
        it "Should have an identity with bottom" $ do
            (bottom <> top) `shouldBe` (top :: Type)
            (bottom <> simpleType String) `shouldBe` (simpleType String :: Type)
        it "Should be commutative" $ do
            simpleType String <> simpleType Number `shouldBe` (simpleType Number <> simpleType String :: Type)
    describe "isWithin'" $ do
        it "Should class anything being within Top" $
               (top `isWithin'` top)
            && (bottom `isWithin'` top)
            && (simpleType String `isWithin'` top)
            && ((simpleType String <> simpleType Number) `isWithin'` top)
        it "(property) items should always be within their merged counterpart" $
            forAll (arbitrary :: Gen (Type, Type)) (\(l, r) -> l `isWithin'` (l <> r) && r `isWithin'` (l <> r))
        it "(property) transitivity" $
            forAll (arbitrary :: Gen (Type, Type, Type)) (\(a, b, c) -> (a `isWithin'` b && b `isWithin'` c) `implies` (a `isWithin'` c))
        it "(property) reflexivity" $
            forAll (arbitrary :: Gen Type) (\x -> x `isWithin'` x)
        describe "Simple type" $ do
            it "Should class two types that are equal to be within it" $
                simpleType String `isWithin'` simpleType String
            it "Should class two simple types that are not equal to not be within each other" $
                not $ simpleType String `isWithin'` simpleType Number
        describe "Constant types" $ do
            it "Should class two equal constant types to be within each other" $
                constNumber 1 `isWithin'` constNumber 1
                && constString "a" `isWithin'` constString "a"
                && constBool True `isWithin'` constBool True
            it "Should class two unequal constant type values of the same type to be within" $
                not (constNumber 1 `isWithin'` constNumber 2)
                && not (constString "a" `isWithin'` constString "b")
                && not (constBool True `isWithin'` constBool False)
            it "Should class two constant types values of different types to be within" $
                not (constNumber 1 `isWithin'` constString "a")
        describe "Tuple types" $ do
            it "Should count two tuple types that are equal to each other as being within each other" $
                tuple [constNumber 1, constString "a"] `isWithin'` tuple [constNumber 1, constString "a"]
            it "Should count a tuple type with smaller element types as being within another" $
                tuple [constNumber 1, constString "a"] `isWithin'` tuple [constNumber 1, simpleType String]
            it "Should count a tuple type with fewer elements types as being within another" $
                tuple [simpleType String, simpleType Number] `isWithin'` tuple [simpleType String, simpleType Number, simpleType Array]
            it "Should count any tuple type as being within a simple array type" $
                tuple [simpleType String] `isWithin'` simpleType Array
            it "Should count a unary tuple type as being an array of that type" $
                tuple [constString "a"] `isWithin'` array (simpleType String)
            it "Should not count a non-unary tuple as being within an array of that type" $
                not (tuple [] `isWithin'` array top)
                && not (tuple [simpleType String, simpleType Number] `isWithin'` array top)
        describe "Array types" $ do
            it "Should not be able to transform an array into a tuple" $
                not (array (simpleType String) `isWithin'` tuple [simpleType  String])
            it "Should count any array as being within the Array simple type" $
                array top `isWithin'` simpleType Array
            it "Should match an array as being within another array when the latter has a wider element type" $
                array (simpleType String) `isWithin'` array top
        describe "Function types" $ do
            it "Should match valid functions with the same argument count as being within" $
                code [simpleType String, simpleType Array] (simpleType String) `isWithin'` code [simpleType String, array top] top
            it "Should define functions as being within if the wider function has more arguments than the smaller" $
                code [simpleType String] (simpleType Number) `isWithin'` code [simpleType String, simpleType Number] (simpleType Number)
            it "Should define functions as being within if the wider function has less arguments than the smaller, and all the missing wider arguments support Nil" $
                code [simpleType String, simpleType Nil] (simpleType Number) `isWithin'` code [simpleType String] (simpleType Number)
        describe "Record type" $ do
            it "Should widen into a simple HashMap" $ do
                record mempty `isWithin'` simpleType HashMap
            it "Should count an empty record as being within itself" $ do
                record (mempty::Map String Type) `isWithin'` record mempty
            it "Should count a record with more fields as being within another" $ do
                record (Map.fromList [("a", top), ("b", top)]) `isWithin'` record (Map.fromList [("a", top)])
            it "Should not count a record with less fields as being within another" $ do
                not $ record (Map.fromList [("a", top)]) `isWithin'` record (Map.fromList [("a", top), ("b", top)])
            it "Should ensure that subtype's fields are subtypes of the supertype's definitions" $
                not $ record (Map.singleton "a" top) `isWithin'` record (Map.singleton "a" bottom)
    describe "intersect" $ do
        it "(property) intersection should be within both types" $ do
            forAll (arbitrary :: Gen (Type, Type)) (\(l, r) -> (l `intersect` r) `isWithin'` l && (l `intersect` r) `isWithin'` r)
        it "(property) intersection with self is an identity" $ do
            forAll arbitrary (\x -> (x `intersect` x) `withinEachOther` x)
        it "(property) intersection + union forms an identity" $ do
            forAll arbitrary (\(l, r) -> (l <> (l `intersect` r)) `withinEachOther` l)
