module TQF.VNISpec where

import TQF.VNI
import TQF.AST
import Test.Hspec
import Test.QuickCheck
import TQF.TypeChecker
import Helpers

prop_escapeUnescapeIdentity s = s == unescapeString (escapeString s)

genValidString :: Gen String
genValidString = listOf $ elements validChars

validChars :: String
validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "<>,._"

escapeSpec = do
    it "Escaping and unescaping a string should be an identity" $
        property $ forAll genValidString prop_escapeUnescapeIdentity

declVNISpec = do
    describe "Variable Declaration" $ do
        it "Variable at root module should have no prefix." $
            vniForDecl [] (VariableDecl voidType $ l "testVar") `shouldBe` "testVar"
        it "Variable at other modules should have module as prefix" $
            vniForDecl [u "Module", u "SubModule"] (VariableDecl voidType $ l "testVar") `shouldBe` "Module_SubModule_testVar"
    describe "Function declaration" $ do
        it "Function at root module should only have fnc prefix" $
            vniForDecl [] (FunctionDecl [] (l "funcName") voidType [] [(stringType, l "arg1"), (numType, l "arg2")] $ CodeBlock [])
                `shouldBe` "fnc_funcName__String_Num"
        it "Function at other modules should have module as prefix" $
            vniForDecl [u "Module", u "SubModule"] (FunctionDecl [] (l "funcName") voidType [] [(stringType, l "arg1"), (numType, l "arg2")] $ CodeBlock [])
                `shouldBe` "Module_SubModule_fnc_funcName__String_Num"
        it "Extern functions should have no type suffix" $
            vniForDecl [u "Module", u "SubModule"] (FunctionDecl [QualifierExtern] (l "funcName") voidType [] [(stringType, l "arg1"), (numType, l "arg2")] $ CodeBlock [])
                `shouldBe` "Module_SubModule_fnc_funcName"

spec = do
  describe "Escape-Unescape string" escapeSpec
  describe "Declaration VNI" declVNISpec