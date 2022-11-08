{-# LANGUAGE FlexibleInstances #-}
module TQF.TypeSpec
    ( spec
    ) where
import Test.Hspec
import Test.QuickCheck
import TQF.Type
import Test.QuickCheck (Arbitrary(arbitrary))

instance Arbitrary Type where
    arbitrary = frequency
        [ (1, return Top)
        , (9, arbitrary)
        ]
instance Arbitrary BaseType where
    arbitrary = oneof
        [ SimpleType <$> elements [minBound..maxBound]
        ]
    
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
        it "Should class anything being within Top" $ do
            shouldSatisfy (top `isWithin` top) id
            shouldSatisfy (bottom `isWithin` top) id
            shouldSatisfy (simpleType String `isWithin` top) id
            shouldSatisfy ((simpleType String <> simpleType Number) `isWithin` top) id
        it "(property) items should always be within their merged counterpart" $ do
            let prop = ((\(l, r) -> l `isWithin` (l <> r) && r `isWithin` (l <> r)) :: (Type, Type) -> Bool)
            forAll arbitrary prop