module TQF.TypeCheckerSpec where

import TQF.AST
import Test.Hspec
import TQF.TypeChecker

typN :: String -> Type
typN = Type [] . TypeName
varN :: String -> Var
varN = Var [] . VarName

typN' :: [String] -> String -> Type
typN' args = Type (map TypeName args) . TypeName
varN' :: [String] -> String -> Var
varN' args = Var (map TypeName args) . VarName

u :: String -> TypeName
u = TypeName
l :: String -> VarName
l = VarName

exprTypeCheckSpec :: Spec
exprTypeCheckSpec = do
    return ()

spec :: Spec
spec = do
    describe "Expression type checking" exprTypeCheckSpec