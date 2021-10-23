module TQF.ModuleResolverSpec (spec) where

import Test.Hspec
import qualified Data.Map.Lazy as Map
import TQF.AST
import TQF.ModuleResolver

data NamespaceLiteral = NamespaceLiteral [(String, Declaration)] [(String, Either NamespaceLiteral TypeName)]

namespace :: NamespaceLiteral -> Namespace
namespace = namespace' []
    where
        namespace' path (NamespaceLiteral lowerIdents upperIdents) = Namespace path lowerIdents' upperIdents'
            where
                lowerIdents' = Map.fromList $ map (mapFst l) lowerIdents
                upperIdents' = Map.fromList $ map upperIdentFunc upperIdents
                upperIdentFunc (ident, Right typ) = (u ident, Right typ)
                upperIdentFunc (ident, Left child) = (u ident, Left $ namespace' (path ++ [u ident]) child)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

u :: String -> TypeName
u = TypeName
l :: String -> VarName
l = VarName


typeVal :: [String] -> String -> Type
typeVal modules = Type (TypeName <$> modules) . TypeName
varVal :: [String] -> String -> Var
varVal modules = Var (TypeName <$> modules) . VarName

addModuleSpec = do
    it "Should add module declarations for a qualified import" $
        shouldResolve
            (ImportStatement [u "Test"] True Nothing)
            (Module [u "Test"] [] [decl1, decl2])
            initialNamespace
            (namespace $ NamespaceLiteral [] [("Test", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])
    it "Should handle nested imports" $
        shouldResolve
            (ImportStatement [u "Nested", u "Test"] True Nothing)
            (Module [u "Nested", u "Test"] [] [decl1, decl2])
            initialNamespace
            (namespace $ NamespaceLiteral [] [("Nested", Left $ NamespaceLiteral [] [("Test", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])])
    it "Should add module declarations to the root namespace as well if unqualified" $
        shouldResolve
            (ImportStatement [u "Test"] False Nothing)
            (Module [u "Test"] [] [decl1, decl2])
            initialNamespace
            (namespace $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [("Test", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])
    it "Should be able to handle multiple nested declarations (Module and Module.Nested)" $ do
        -- Module - func1, func2. Module.Nested - func3
        shouldResolve
            (ImportStatement [u "Module"] True Nothing)
            (Module [u "Module"] [] [decl1, decl2])
            (namespace $ NamespaceLiteral [] [("Module", Left $ NamespaceLiteral [] [("Nested", Left $ NamespaceLiteral [("func3", decl3)] [])])])
            (namespace $ NamespaceLiteral [] [("Module", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [("Nested", Left $ NamespaceLiteral [("func3", decl3)] [])])])
        shouldResolve
            (ImportStatement [u "Module", u "Nested"] True Nothing)
            (Module [u "Module", u "Nested"] [] [decl3])
            (namespace $ NamespaceLiteral [] [("Module", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])
            (namespace $ NamespaceLiteral [] [("Module", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [("Nested", Left $ NamespaceLiteral [("func3", decl3)] [])])])
    it "Should be able to handle renaming imports" $
        shouldResolve
            (ImportStatement [u "Module"] True (Just [u "Renamed"]))
            (Module [u "Module"] [] [decl1, decl2])
            initialNamespace
            (namespace $ NamespaceLiteral [] [("Renamed", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])
    it "Should error when an ident overwrites another" $
        shouldError
            (ImportStatement [u "Test"] True Nothing)
            (Module [u "Test"] [] [decl1, decl2])
            (namespace $ NamespaceLiteral [] [("Test", Left $ NamespaceLiteral [("func1", decl1), ("func2", decl2)] [])])
            (LIdentClash [u "Test"] (l "func1"))
    it "Should error when an ident overwrites another" $
        shouldError
            (ImportStatement [u "Test"] True Nothing)
            (Module [u "Test"] [] [decl1, decl2])
            (namespace $ NamespaceLiteral [] [("Test", Right $ u "TypeName")])
            (NamespaceTypeClash $ Type [] (u "Test"))
    where

        decl1 = Function [QualifierExtern] (l "func1") (typeVal [] "Void") [] [] (CodeBlock [])
        decl2 = Function [] (l "func2") (typeVal [] "String") [] [] (CodeBlock [])
        decl3 = Function [] (l "func3") (typeVal [] "String") [] [(typeVal [] "Num", l "arg")] (CodeBlock [])


        shouldResolve statement ast input expectedOutput = runNamespaceTransformation (addModule statement ast) input `shouldBe` Right expectedOutput
        shouldError statement ast input expectedError = runNamespaceTransformation (addModule statement ast) input `shouldBe` Left expectedError
spec = do
    describe "addModule" addModuleSpec