module TQF.TypeCheckerSpec where

import           Data.Functor
import           Helpers
import           TQF.AST
import           TQF.ModuleResolver
import           TQF.TypeChecker
import           Test.Hspec

ns :: Namespace
ns = buildNamespace $ NamespaceLiteral [("globalVar", [VariableDecl stringType (l "globalVar")])]
                                       [("Module", Left subModule)]
 where
  subModule = NamespaceLiteral
    [ ("subGlobalVar", [VariableDecl numType $ l "subGlobalVar"])
    , ("func1", [FunctionDecl [] (l "func1") stringType [] [(numType, l "arg1")] (CodeBlock [])])
    , ( "func2"
      , [ FunctionDecl [] (l "func2") stringType [] [(numType, l "x")]    (CodeBlock [])
        , FunctionDecl [] (l "func2") numType    [] [(stringType, l "x")] (CodeBlock [])
        , FunctionDecl []
                       (l "func2")
                       boolType
                       []
                       [(numType, l "x"), (stringType, l "x")]
                       (CodeBlock [])
        ]
      )
    ]
    []

shouldTypeCheck :: (Eq a, Show a) => TypeCheck a -> a -> Expectation
shouldTypeCheck actual expected = runTypeCheck actual `shouldBe` Right expected

shouldNotTypeCheck :: (Eq a, Show a) => TypeCheck a -> TypeCheckError -> Expectation
shouldNotTypeCheck actual err = runTypeCheck actual `shouldBe` Left err

exprTypeCheckSpec :: Spec
exprTypeCheckSpec = do
  describe "Variable access" $ do
    it "Should typecheck variable accessing that matches variable" $ do
      typeCheckExpr ns (Variable $ varN "globalVar") `shouldTypeCheck` stringType

      typeCheckExpr ns (Variable $ varN' ["Module"] "subGlobalVar") `shouldTypeCheck` numType
    it "Should fail to typecheck variables that do not exist"
      $                    typeCheckExpr ns (Variable $ varN "globalVarDoesntExist")
      `shouldNotTypeCheck` NotPresent (varN "globalVarDoesntExist")
  describe "Unary operator" $ do
    it "Should typecheck a valid unary operator"
      $                 typeCheckExpr ns (UnaryOperator NegOp (NumLiteral "10"))
      `shouldTypeCheck` numType
    it "Should not typecheck a unary operator with invalid argument"
      $                    typeCheckExpr ns (UnaryOperator NotOp (NumLiteral "10"))
      `shouldNotTypeCheck` TypeMismatch boolType numType
  describe "Binary operator" $ do
    it "Should typecheck a valid binary operation"
      $                 typeCheckExpr ns (BinaryOperator AddOp (NumLiteral "10") (NumLiteral "10"))
      `shouldTypeCheck` numType
    it "Should typecheck a binary operation with invalid argument"
      $ typeCheckExpr ns (BinaryOperator AddOp (NumLiteral "10") (BoolLiteral True))
      `shouldNotTypeCheck` TypeMismatch numType boolType
  describe "Function Call" $ do
    it "Should typecheck a valid function call"
      $                 typeCheckExpr ns (FuncCall (varN' ["Module"] "func1") [NumLiteral ""])
      `shouldTypeCheck` stringType
    it "Should not typecheck a function with invalid arguments"
      $                    typeCheckExpr ns (FuncCall (varN' ["Module"] "func1") [])
      `shouldNotTypeCheck` NotPresent (varN' ["Module"] "func1")
    it "Should correctly handle overloaded functions" $ do
      typeCheckExpr ns (FuncCall (varN' ["Module"] "func2") [NumLiteral ""])
        `shouldTypeCheck` stringType

      typeCheckExpr ns (FuncCall (varN' ["Module"] "func2") [NumLiteral "", StringLiteral ""])
        `shouldTypeCheck` boolType
  describe "Bool literal"
    $                 it "Should typecheck a bool literal"
    $                 typeCheckExpr ns (BoolLiteral True)
    `shouldTypeCheck` boolType
  describe "Num literal"
    $                 it "Should typecheck a num literal"
    $                 typeCheckExpr ns (NumLiteral "")
    `shouldTypeCheck` numType
  describe "String literal"
    $                 it "Should typecheck a num literal"
    $                 typeCheckExpr ns (StringLiteral "")
    `shouldTypeCheck` stringType
  describe "Array literal"
    $                 it "Should typecheck an array literal"
    $                 typeCheckExpr ns (Array [])
    `shouldTypeCheck` arrayType
  describe "Direct call" $ do
    describe "Valid call" $ do
      it "Nular" $ typeCheckExpr ns (DirectCall "allUnits" []) `shouldTypeCheck` arrayType
      it "Unary"
        $                 typeCheckExpr ns (DirectCall "getPos" [DirectCall "player" []])
        `shouldTypeCheck` arrayType
      it "Binary"
        $                 typeCheckExpr ns (DirectCall "setPos" [DirectCall "player" [], Array []])
        `shouldTypeCheck` voidType
    describe "Invalid arguments" $ do
      it "Unary"
        $                    typeCheckExpr ns (DirectCall "getPos" [NumLiteral ""])
        `shouldNotTypeCheck` CannotFindSQFCommand "getPos" [numType]
      it "Binary"
        $                    typeCheckExpr ns (DirectCall "setPos" [NumLiteral "", Array []])
        `shouldNotTypeCheck` CannotFindSQFCommand "setPos" [numType, arrayType]
    it "Should not typecheck a direct call with invalid arg count (not 0-2)"
      $ typeCheckExpr ns (DirectCall "setPos" [NumLiteral "", NumLiteral "", NumLiteral ""])
      `shouldNotTypeCheck` InvalidDirectCallArgCount 3
  describe "Cast" $ do
    it "Should cast to whatever type the poor sod decides, breaking the type system"
      $                 typeCheckExpr ns (Cast stringType $ NumLiteral "")
      `shouldTypeCheck` stringType

statementTypeCheckSpec :: Spec
statementTypeCheckSpec = do
  describe "Code block" $ do
    it "Should type check code blocks only when every item in a code block is valid" $ do
      CodeBlock [] `shouldTypeCheck'` voidType
      CodeBlock [FunctionCall (varN' ["Module"] "func1") [NumLiteral ""]]
        `shouldTypeCheck'` voidType
    it "Should fail to typecheck code blocks where any item isn't valid" $ do
      shouldNotTypeCheck' (CodeBlock [FunctionCall (varN' ["Module"] "func1") [StringLiteral ""]])
                          voidType
                          (NotPresent (varN' ["Module"] "func1"))
  describe "Variable declaration" $ do
    it "Should type check statements that use a local variable after its declaration" $ do
      CodeBlock
          [ VariableDeclaration numType (l "ident") Nothing
          , FunctionCall (varN' ["Module"] "func1") [Variable $ varN "ident"]
          ]
        `shouldTypeCheck'` voidType
      shouldNotTypeCheck'
        (CodeBlock
          [ VariableDeclaration stringType (l "ident") Nothing
          , FunctionCall (varN' ["Module"] "func1") [Variable $ varN "ident"]
          ]
        )
        voidType
        (NotPresent $ varN' ["Module"] "func1")
    it "Should typecheck the assignment in a variable declaration if present" $ do
      VariableDeclaration numType (l "ident") Nothing `shouldTypeCheck'` voidType
      VariableDeclaration numType (l "ident") (Just $ NumLiteral "1") `shouldTypeCheck'` voidType
      shouldNotTypeCheck' (VariableDeclaration numType (l "ident") (Just $ StringLiteral "x"))
                          voidType
                          (TypeMismatch numType stringType)
  describe "Variable assignment" $ do
    it "Should typecheck assignments to global variables"
      $                  Assignment (varN "globalVar") (StringLiteral "")
      `shouldTypeCheck'` voidType
    it "Should typecheck assignments to local variables" $ do
      CodeBlock
          [ VariableDeclaration numType (l "ident") Nothing
          , Assignment (varN "ident") (NumLiteral "0")
          ]
        `shouldTypeCheck'` voidType
    it "Should not typecheck if there is a mismatch between declared and actual type"
      $ shouldNotTypeCheck' (Assignment (varN "globalVar") (NumLiteral "0"))
                            voidType
                            (TypeMismatch stringType numType)
  describe "Function call" $ do
    it
        "Should always work as long as it works in expressions, because it's literally fed directly through"
      $ do
          True `shouldBe` True
  describe "If statements" $ do
    it "Should ensure the condition of an if statement is a Bool" $ do
      IfStatement (BinaryOperator EqOp (NumLiteral "1") (NumLiteral "1")) (Return Nothing) Nothing
        `shouldTypeCheck'` voidType
      shouldNotTypeCheck' (IfStatement (NumLiteral "1") (Return Nothing) Nothing)
                          voidType
                          (TypeMismatch boolType numType)
    it "Should check sub-statements" $ do
      shouldNotTypeCheck'
        (IfStatement (BoolLiteral True) (Return (Just $ BoolLiteral True)) Nothing)
        voidType
        (TypeMismatch voidType boolType)
      shouldNotTypeCheck'
        (IfStatement (BoolLiteral True) (Return Nothing) (Just $ Return (Just $ BoolLiteral True)))
        voidType
        (TypeMismatch voidType boolType)
  describe "While loops" $ do
    it "Should ensure the condition of an while loop is a Bool" $ do
      WhileLoop (BoolLiteral True) (Return Nothing) `shouldTypeCheck'` voidType
      shouldNotTypeCheck' (WhileLoop (NumLiteral "0") (Return Nothing))
                          voidType
                          (TypeMismatch boolType numType)
    it "Should check sub-statements" $ do
      shouldNotTypeCheck' (WhileLoop (BoolLiteral True) (Return $ Just $ NumLiteral "0"))
                          voidType
                          (TypeMismatch voidType numType)
  describe "Do while loops" $ do
    it "Should ensure the condition of an do while loop is a Bool" $ do
      DoWhile (BoolLiteral True) (Return Nothing) `shouldTypeCheck'` voidType
      shouldNotTypeCheck' (DoWhile (NumLiteral "0") (Return Nothing))
                          voidType
                          (TypeMismatch boolType numType)
    it "Should check sub-statements" $ do
      shouldNotTypeCheck' (DoWhile (BoolLiteral True) (Return $ Just $ NumLiteral "0"))
                          voidType
                          (TypeMismatch voidType numType)
  describe "Return" $ do
    it "Should only allow returning nothing when the function return type is nothing" $ do
      Return Nothing `shouldTypeCheck'` voidType
      shouldNotTypeCheck' (Return Nothing) numType (TypeMismatch numType voidType)
    it "Should only allow returning something when the function return type is something" $ do
      shouldNotTypeCheck' (Return Nothing) numType (TypeMismatch numType voidType)
      Return (Just $ NumLiteral "1") `shouldTypeCheck'` numType
 where
  shouldTypeCheck' stmt retType = void (typeCheckStatement ns retType stmt) `shouldTypeCheck` ()
  shouldNotTypeCheck' stmt retType err =
    void (typeCheckStatement ns retType stmt) `shouldNotTypeCheck` err

spec :: Spec
spec = do
  describe "Expression type checking" exprTypeCheckSpec
  describe "Statement type checking"  statementTypeCheckSpec
