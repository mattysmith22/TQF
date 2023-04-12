{-# LANGUAGE TupleSections #-}
module TQF.ParserSpec
  ( spec
  ) where

import           Helpers
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Lexer
import           TQF.Parser
import           TQF.Type
import           Test.Hspec

shouldParse' :: String -> Module Parsed -> Expectation
shouldParse' input ast = runAlex input parse `shouldBe` Right ast

testParse :: String -> Either String (Module Parsed)
testParse inp = runAlex inp parse

a :: a -> Annot a
a = noPlace

moduleSpec :: SpecWith ()
moduleSpec = do
  it "Should parse a module heading"
    $              "module ModuleName where"
    `shouldParse'` Module [u "ModuleName"] [] []
  it "Should parse a module heading with nested namespacing"
    $              "module Test.ModuleName where"
    `shouldParse'` Module [u "Test", u "ModuleName"] [] []

importSpec :: SpecWith ()
importSpec = do
  it "Should parse a normal import statement"
    $             "import ImportModule"
    `shouldParse` [ImportStatement [u "ImportModule"] False Nothing]
  it "Should parse multiple import statements"
    $             "import ImportModule\nimport AnotherModule"
    `shouldParse` [ ImportStatement [u "ImportModule"]  False Nothing
                  , ImportStatement [u "AnotherModule"] False Nothing
                  ]
  it "Should parse qualified imports"
    $             "import qualified ImportModule"
    `shouldParse` [ImportStatement [u "ImportModule"] True Nothing]
  it "Should parse renaming imports"
    $             "import qualified ImportModule as Renamed"
    `shouldParse` [ImportStatement [u "ImportModule"] True (Just [u "Renamed"])]
 where
  shouldParse inp ast = testParse ("module Test where " ++ inp)
    `shouldBe` Right (Module [u "Test"] ast [])

declarationSpec :: SpecWith ()
declarationSpec = do
  it "Should parse a function declaration" $ do
    "function functionName(input: nil): string {}"
      `shouldParse` [ a $ FunctionDecl (a $ l "functionName")
                                   (a $ ParsedType $ simpleType String)
                                   []
                                   [(a $ ParsedType $ simpleType Nil, a $ l "input")]
                                   []
                    ]
  it "Should parse a function declaration with namespaced types" $ do
    "function functionName(input: NS.Void): NS.String {}"
      `shouldParse` [ a $ FunctionDecl (a $ l "functionName")
                                   (a $ ParsedType $ extra (UIdent [u "NS"] (u "String"), []))
                                   []
                                   [(a $ ParsedType $ extra (UIdent [u "NS"] (u "Void"), []), a $ l "input")]
                                   []
                    ]
  it "Should parse a function declaration with multiple arguments" $ do
    "function functionName(input: nil, input2: num): string {}"
      `shouldParse` [ a $ FunctionDecl
                        (a $ l "functionName")
                        (a $ ParsedType $ simpleType String)
                        []
                        [(a $ ParsedType $ simpleType Nil, a $ l "input"), (a $ ParsedType $ simpleType Number, a $ l "input2")]
                        []
                    ]
  it "Should parse multiple function declarations"
    $             "function functionName(input: nil): string {} function functionName2(input2: string): num {}"
    `shouldParse` [ a $ FunctionDecl (a $ l "functionName")
                                 (a $ ParsedType $ simpleType String)
                                 []
                                 [(a $ ParsedType $ simpleType Nil, a $ l "input")]
                                 []
                  , a $ FunctionDecl (a $ l "functionName2")
                                 (a $ ParsedType $ simpleType Number)
                                 []
                                 [(a $ ParsedType $ simpleType String, a $ l "input2")]
                                 []
                  ]
  it "Should parse an external function declarations"
    $ "external function functionName(input: nil, input2: num): bool = \"test\""
    `shouldParse` [ a $ ExternalFunctionDecl
        (a $ l "functionName")
        (a $ ParsedType $ simpleType Bool)
        []
        [(a $ ParsedType $ simpleType Nil, a $ l "input"), ( a $ ParsedType $ simpleType Number, a $ l "input2")]
        "test"
        ]
  it "Should parse an external variable declaration"
    $ "external varName: string = \"test\""
    `shouldParse` [ a $ ExternalVariableDecl
        (a $ l "varName")
        ( a $ ParsedType $ simpleType String)
        "test"
        ]
 where
  shouldParse inp ast = testParse ("module Test where " ++ inp)
    `shouldBe` Right (Module [u "Test"] [] ast)

statementSpec :: SpecWith ()
statementSpec = do
  describe "Variable Declarations" $ do
    it "Should parse a variable declaration without an assignment"
      $             "var varName: Type;"
      `shouldParse` [a $ VariableDeclaration (a $ ParsedType $ extra $ (,[]) $ typN' [] "Type") (a $ l "varName") Nothing]
    it "Should parse a variable declaration with an assignment"
      $             "var varName: Type = 1;"
      `shouldParse` [a $ VariableDeclaration ( a $ ParsedType $ extra $ (,[]) $ typN' [] "Type") (a $ l "varName") (Just $ a $ NumLiteral 1)]
  describe "Assignment"
    $             it "Should parse an assignment"
    $             "varName = 1;"
    `shouldParse` [a $ Assignment (a $ Variable $ a $ Ident (varN' [] "varName") []) (a $ NumLiteral 1)]
 where
  shouldParse inp ast =
    testParse("module Test where function func(): nil {" ++  inp ++ "}")
      `shouldBe` Right
                   (Module [u "Test"]
                           []
                           [a $ FunctionDecl (a $ l "func") (a $ ParsedType $ simpleType Nil) [] [] ast]
                   )

expressionSpec :: SpecWith ()
expressionSpec = do
  describe "Variables" $ do
    it "Should parse a normal variable reading" $ "variable" `shouldParse` Variable
      ( a $ Ident (varN' [] "variable") [])
    it "Should parse a variable reading and Namespaces"
      $             "Namespace.variable"
      `shouldParse` Variable ( a $ Ident (varN' ["Namespace"] "variable") [])
  describe "Literals" $ do
    it "Should parse bool literals" $ do
      "true" `shouldParse` BoolLiteral True
      "false" `shouldParse` BoolLiteral False
    it "Should parse normal integer literals" $ do
      "15" `shouldParse` NumLiteral 15
      "0" `shouldParse` NumLiteral 0
      "-15" `shouldParse` UnOp (a NegOp) (a $ NumLiteral 15)
    it "Should parse normal decimal literals" $ do
      "0.15" `shouldParse` NumLiteral 0.15
    it "Should parse normal hex literals" $ "0x1a5" `shouldParse` NumLiteral 421
    it "Should parse string literals" $ do
      "\"Test\"" `shouldParse` StringLiteral "Test"
      "\"String with Sp@ces\"" `shouldParse` StringLiteral "String with Sp@ces"
      "\"String \\n with \\\" escape \\t codes\""
        `shouldParse` StringLiteral "String \\n with \\\" escape \\t codes"
  describe "Unary Operators" $ do
    it "Should parse negate operators" $ do
      "-(20)" `shouldParse` UnOp (a NegOp) (a $ NumLiteral 20)
    it "Should parse not operators" $ do
      "!true" `shouldParse` UnOp (a NotOp) (a $ BoolLiteral True)
  describe "Binary Operators" $ do
    it "Should parse mathematical operators" $ do
      "1+2" `shouldParse` BinOp (a AddOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1-2" `shouldParse` BinOp (a SubOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1*2" `shouldParse` BinOp (a MulOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1/2" `shouldParse` BinOp (a DivOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1%2" `shouldParse` BinOp (a ModOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
    it "Should parse boolean operators" $ do
      "1&&2" `shouldParse` BinOp (a AndOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1||2" `shouldParse` BinOp (a OrOp)  (a $ NumLiteral 1) (a $ NumLiteral 2)
    it "Should parse comparison operators" $ do
      "1==2" `shouldParse` BinOp (a EqOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1!=2" `shouldParse` BinOp (a NotEqOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1<2" `shouldParse` BinOp (a LessOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1>2" `shouldParse` BinOp (a GreaterOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1>=2" `shouldParse` BinOp (a GreaterEqualOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
      "1<=2" `shouldParse` BinOp (a LessEqualOp) (a $ NumLiteral 1) (a $ NumLiteral 2)
  describe "FunctionDecl calls" $ do
    it "Should parse a function call" $ "func(1, 2)" `shouldParse` FuncCall
      ( a $ Variable $  a $ Ident (varN' [] "func") [])
      [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "Array construction" $ do
    it "Should parse an empty array" $ "[]" `shouldParse` ArrayExpr []
    it "Should parse a single array" $ "[1]" `shouldParse` ArrayExpr [ a $ NumLiteral 1]
    it "Should parse multiple elements of an array" $ "[1, 2]" `shouldParse` ArrayExpr
      [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "Type cast" $ it "Should parse a type cast" $ "<NS.Type>1" `shouldParse` Cast
    ( a $ ParsedType $ extra (typN' ["NS"] "Type", []))
    ( a $ NumLiteral 1)
  describe "Precedence" $ do
    it "Precedence test 1" $ "1 + 2 * 3" `shouldParse` BinOp (a AddOp)
      (a $ NumLiteral 1)
      (a $ BinOp (a MulOp) (a $ NumLiteral 2) (a $ NumLiteral 3))
    it "Precedence test 2" $ "(1 + 2) * 3" `shouldParse` BinOp (a MulOp)
      (a $ BinOp (a AddOp) (a $ NumLiteral 1) (a $ NumLiteral 2))
      (a $ NumLiteral 3)
    it "Precedence test 3" $ "(1 + -2) * 3" `shouldParse` BinOp (a MulOp)
      (a $ BinOp (a AddOp) (a $ NumLiteral 1) (a $ UnOp (a NegOp) (a $ NumLiteral 2)))
      (a $ NumLiteral 3)
  describe "While statement" $ do
    it "Should parse a normal while loop"
      $             "while (true) {false;}"
      `shouldParse` WhileLoop (a $ BoolLiteral True) [a $ Expr $ a $ BoolLiteral False]
  describe "If Statement" $ do
    it "Should parse a normal if statement"
      $             "if (true) then {false;}"
      `shouldParse` IfStatement (a $ BoolLiteral True) (ThenDo
                                  [a $ Expr $ a $ BoolLiteral False]
                                  Nothing)

    it "Should parse an if statement with an else statement"
      $             "if (true) then {\"a\";} else {\"b\";}"
      `shouldParse` IfStatement (a $ BoolLiteral True) (ThenDo
                        [a $ Expr $ a $ StringLiteral "a"]
                        (Just [a $ Expr $ a $ StringLiteral "b"]))
    it "Should parse an if statement with an exitWith statement"
      $             "if (true) exitWith {\"a\";}"
      `shouldParse` IfStatement (a $ BoolLiteral True) (ThenExitWith [a $ Expr $ a $ StringLiteral "a"])
 where
  shouldParse inp ast =
    testParse ("module Test where function func(): nil {" ++  inp ++ ";}")
      `shouldBe` Right
                   (Module
                     [u "Test"]
                     []
                     [a $ FunctionDecl
                                    (a $ l "func")
                                    (a $ ParsedType $ simpleType Nil)
                                    []
                                    []
                                    [a $ Expr $ a ast]
                     ]
                   )

spec :: SpecWith ()
spec = parallel $ do
  describe "Module declaration" moduleSpec
  describe "Import declaration" importSpec
  describe "Declarations"       declarationSpec
  describe "Statements"         statementSpec
  describe "Expressions"        expressionSpec
