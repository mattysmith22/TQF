{-# LANGUAGE FlexibleInstances #-}
module TQF.TypeSpec
    ( spec
    ) where
import Test.Hspec
import Test.QuickCheck
import TQF.Type

spec :: Spec
spec = do
    describe "monoid" $ do
        it "Anything merged with Top is Top" $ do
            (top <> top) `shouldBe` top
            (top <> bottom) `shouldBe` top
            (top <> bottom) `shouldBe` top
        it "Should have an identity with bottom" $ do
            (bottom <> top) `shouldBe` top
            (bottom <> simpleType String) `shouldBe` simpleType String
        it "Should be commutative" $ do
            simpleType String <> simpleType Number `shouldBe` simpleType Number <> simpleType String
    describe "isWithin" $ do
        it "Should class anything being within Top" $
               (top `isWithin` top)
            && (bottom `isWithin` top)
            && (simpleType String `isWithin` top)
            && ((simpleType String <> simpleType Number) `isWithin` top)
        it "(property) items should always be within their merged counterpart" $
            forAll (arbitrary :: Gen (Type, Type)) (\(l, r) -> l `isWithin` (l <> r) && r `isWithin` (l <> r))
    describe "Simple type" $ do
        it "Should class two types that are equal to be within it" $
            simpleType String `isWithin` simpleType String
        it "Should class two simple types that are not equal to not be within each other" $
            not $ simpleType String `isWithin` simpleType Number
    describe "Constant types" $ do
        it "Should class two equal constant types to be within each other" $
            constNumber 1 `isWithin` constNumber 1
            && constString "a" `isWithin` constString "a"
            && constBool True `isWithin` constBool True
        it "Should class two unequal constant type values of the same type to be equal" $
            not (constNumber 1 `isWithin` constNumber 2)
            && not (constString "a" `isWithin` constString "b")
            && not (constBool True `isWithin` constBool False)
        it "Should class two constant types values of different types to nto be within" $
            not (constNumber 1 `isWithin` constString "a")