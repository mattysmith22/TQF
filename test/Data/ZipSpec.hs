module Data.ZipSpec (spec) where

import           Data.Zip
import           Test.Hspec

spec :: Spec
spec = do
    describe "zipPadded" $ do
        it "zipPadded dl dr [] [] = []" $
            zipPadded "a" "b" [] [] `shouldBe` []
        it "zipPadded dl dr [x] [] = [(x, dr)]" $
            zipPadded "a" "b" ["a1"] [] `shouldBe` [("a1", "b")]
        it "zipPadded dl dr [] [x] = [(dl, x)]" $
            zipPadded "a" "b" [] ["b1"] `shouldBe` [("a", "b1")]
        it "zipPadded dl dr [x] [y] = [(x,y)]" $
            zipPadded "a" "b" ["a1"] ["b1"] `shouldBe` [("a1", "b1")]
        it "zipPadded dl dr [x] [y,z] = [(x,y),(dl,z)]" $
            zipPadded "a" "b" ["a1"] ["b1", "b2"] `shouldBe` [("a1", "b1"), ("a", "b2")]
