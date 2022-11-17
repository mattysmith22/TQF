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
shouldParse' input ast = (runAlex input parse) `shouldBe` Right ast

unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = loop id
    where
        loop f = do
            x <- m
            if p x
                then loop (f . (x:))
                else return (f [])

alexScanTokens inp = either error id $ runAlex inp (unfoldWhileM (/=TokenEOF) alexMonadScan)

testParse :: String -> Either String (Module Parsed)
testParse inp = runAlex inp parse

a :: a -> Annot a
a = noPlace

moduleSpec = do
  it "Should parse a module heading"
    $              "module ModuleName where"
    `shouldParse'` Module [u "ModuleName"] [] []
  it "Should parse a module heading with nested namespacing"
    $              "module Test.ModuleName where"
    `shouldParse'` Module [u "Test", u "ModuleName"] [] []

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

declarationSpec = do
  it "Should parse a function declaration" $ do
    "function functionName(input: nil): string {}"
      `shouldParse` [ a $ FunctionDecl (l "functionName")
                                   (a $ simpleType String)
                                   [(a $ simpleType Nil, l "input")]
                                   (a $ CodeBlock [])
                    ]
  it "Should parse a function declaration with namespaced types" $ do
    "function functionName(input: NS.Void): NS.String {}"
      `shouldParse` [ a $ FunctionDecl (l "functionName")
                                   (a $ extra $ UIdent [u "NS"] (u "String"))
                                   [(a $ extra $ UIdent [u "NS"] (u "Void"), l "input")]
                                   ( a $ CodeBlock [])
                    ]
  it "Should parse a function declaration with multiple arguments" $ do
    "function functionName(input: nil, input2: num): string {}"
      `shouldParse` [ a $ FunctionDecl
                        (l "functionName")
                        (a $ simpleType String)
                        [(a $ simpleType Nil, l "input"), (a $ simpleType Number, l "input2")]
                        ( a $ CodeBlock [])
                    ]
  it "Should parse multiple function declarations"
    $             "function functionName(input: nil): string {} function functionName2(input2: string): num {}"
    `shouldParse` [ a $ FunctionDecl (l "functionName")
                                 (a $ simpleType String)
                                 [(a $ simpleType Nil, l "input")]
                                 (a $ CodeBlock [])
                  , a $ FunctionDecl (l "functionName2")
                                 (a $ simpleType Number)
                                 [(a $ simpleType String, l "input2")]
                                 (a $ CodeBlock [])
                  ]
  it "Should parse an external function declarations"
    $ "external function functionName(input: nil, input2: num): bool = \"test\""
    `shouldParse` [ a $ ExternalFunctionDecl
        (l "functionName")
        (a $ simpleType Bool)
        [(a $ simpleType Nil, l "input"), ( a $ simpleType Number, l "input2")]
        "test"
        ]
  it "Should parse an external variable declaration"
    $ "external varName: string = \"test\""
    `shouldParse` [ a $ ExternalVariableDecl
        (l "varName")
        ( a $ simpleType String)
        "test"
        ]
 where
  shouldParse inp ast = testParse ("module Test where " ++ inp)
    `shouldBe` Right (Module [u "Test"] [] ast)

statementSpec = do
  describe "FunctionDecl call" $ do
    it "Should parse a normal function call"
      $             "func(1,\"test\");"
      `shouldParse` [ a $ FunctionCall ( a $ toIdent "func") [a $ NumLiteral 1, a $ StringLiteral "test"]]
    it "Should parse an empty function call"
      $             "func();"
      `shouldParse` [ a $ FunctionCall ( a $ toIdent "func") []]
    it "Should parse a function call in another namespace"
      $             "Test.func(1);"
      `shouldParse` [ a $ FunctionCall ( a $ toIdent "Test.func") [a $ NumLiteral 1]]
  describe "Return" $ do
    it "Should parse a normal return statement"
      $             "return 1;"
      `shouldParse` [a $ Return $ Just $ a $ NumLiteral 1]
    it "Should parse a return statement without a value" $ "return;" `shouldParse` [ a $ Return Nothing]
  describe "Code Block" $ do
    it "Should parse a normal empty code block statement" $ "{}" `shouldParse` [ a $ CodeBlock []]
    it "Should parse a normal code block with a single internal statement"
      $             "{return 1;}"
      `shouldParse` [a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 1]]
    it "Should parse a normal code block with multiple internal statements"
      $             "{return 1; return 2;}"
      `shouldParse` [a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 1, a $ Return $ Just $ a $ NumLiteral 2]]
  describe "Variable Declarations" $ do
    it "Should parse a variable declaration without an assignment"
      $             "varName: Type;"
      `shouldParse` [a $ VariableDeclaration (a $ extra $ typN' [] "Type") (l "varName") Nothing]
    it "Should parse a variable declaration with an assignment"
      $             "varName: Type = 1;"
      `shouldParse` [a $ VariableDeclaration ( a $ extra $ typN' [] "Type") (l "varName") (Just $ a $ NumLiteral 1)]
  describe "Assignment"
    $             it "Should parse an assignment"
    $             "varName = 1;"
    `shouldParse` [a $ Assignment ( a $ varN' [] "varName") (a $ NumLiteral 1)]
  describe "If Statement" $ do
    it "Should parse a normal if statement"
      $             "if (true) {return 1;}"
      `shouldParse` [ a $ IfStatement (a $ BoolLiteral True)
                                  (a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 1])
                                  Nothing
                    ]
    it "Should parse an if statement with an else statement"
      $             "if (true) {return 1;} else {return 2;}"
      `shouldParse` [ a $ IfStatement (a $ BoolLiteral True) (a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 1])
                        $ Just (a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 2])
                    ]
    it "Should parse an if statement that doesn't use code blocks" $ do
      "if (true) return 1 else return 2;"
        `shouldParse` [ a $ IfStatement (a $ BoolLiteral True)
                                    ( a $ Return $ Just $ a $ NumLiteral 1)
                                    ( Just $ a $ Return $ Just $ a $ NumLiteral 2)
                      ]
      "if (true) return 1;"
        `shouldParse` [a $ IfStatement (a $ BoolLiteral True) (a $ Return $ Just $ a $ NumLiteral 1) Nothing]
  describe "While statement" $ do
    it "Should parse a normal while loop"
      $             "while (true) {return 1;}"
      `shouldParse` [a $ WhileLoop (a $ BoolLiteral True) (a $ CodeBlock [a $ Return $ Just $ a $ NumLiteral 1])]
    it "Should parse a while loop that doesn't use code blocks"
      $             "while (true) return 1;"
      `shouldParse` [a $ WhileLoop (a $ BoolLiteral True) (a $ Return $ Just $ a $ NumLiteral 1)]
 where
  shouldParse inp ast =
    testParse("module Test where function func(): nil {" ++  inp ++ "}")
      `shouldBe` Right
                   (Module [u "Test"]
                           []
                           [a $ FunctionDecl (l "func") (a $ simpleType Nil) [] (a $ CodeBlock ast)]
                   )

expressionSpec = do
  describe "Variables" $ do
    it "Should parse a normal variable reading" $ "variable" `shouldParse` Variable
      ( a $ varN' [] "variable")
    it "Should parse a variable reading and Namespaces"
      $             "Namespace.variable"
      `shouldParse` Variable ( a $ varN' ["Namespace"] "variable")
  describe "Literals" $ do
    it "Should parse bool literals" $ do
      "true" `shouldParse` BoolLiteral True
      "false" `shouldParse` BoolLiteral False
    it "Should parse normal integer literals" $ do
      "15" `shouldParse` NumLiteral 15
      "0" `shouldParse` NumLiteral 0
      "-15" `shouldParse` DirectCall "-" [a $ NumLiteral 15]
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
      "-(20)" `shouldParse` DirectCall "-" [ a $ NumLiteral 20]
    it "Should parse not operators" $ do
      "!true" `shouldParse` DirectCall "!" [ a $ BoolLiteral True]
  describe "Binary Operators" $ do
    it "Should parse mathematical operators" $ do
      "1+2" `shouldParse` DirectCall "+" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1-2" `shouldParse` DirectCall "-" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1*2" `shouldParse` DirectCall "*" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1/2" `shouldParse` DirectCall "/" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1%2" `shouldParse` DirectCall "%" [ a $ NumLiteral 1, a $ NumLiteral 2]
    it "Should parse boolean operators" $ do
      "1&&2" `shouldParse` DirectCall "&&" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1||2" `shouldParse` DirectCall "||" [ a $ NumLiteral 1, a $ NumLiteral 2]
    it "Should parse comparison operators" $ do
      "1==2" `shouldParse` DirectCall "isEqualTo" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1!=2" `shouldParse` DirectCall "isNotEqualTo" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1<2" `shouldParse` DirectCall "<" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1>2" `shouldParse` DirectCall ">" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1>=2" `shouldParse` DirectCall ">=" [ a $ NumLiteral 1, a $ NumLiteral 2]
      "1<=2" `shouldParse` DirectCall "<=" [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "FunctionDecl calls" $ do
    it "Should parse a function call" $ "func(1, 2)" `shouldParse` FuncCall
      ( a $ varN' [] "func")
      [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "Array construction" $ do
    it "Should parse an empty array" $ "[]" `shouldParse` ArrayExpr []
    it "Should parse a single array" $ "[1]" `shouldParse` ArrayExpr [ a $ NumLiteral 1]
    it "Should parse multiple elements of an array" $ "[1, 2]" `shouldParse` ArrayExpr
      [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "Direct calls" $ do
    it "Should parse a nular direct call" $ "<sqfCommand>()" `shouldParse` DirectCall "sqfCommand" []
    it "Should parse a unary direct call" $ "<sqfCommand>(1)" `shouldParse` DirectCall
      "sqfCommand"
      [ a $ NumLiteral 1]
    it "Should parse a binary direct call" $ "<sqfCommand>(1,2)" `shouldParse` DirectCall
      "sqfCommand"
      [ a $ NumLiteral 1, a $ NumLiteral 2]
  describe "Type cast" $ it "Should parse a type cast" $ "<NS.Type>1" `shouldParse` Cast
    ( a $ extra $ typN' ["NS"] "Type")
    ( a $ NumLiteral 1)
  describe "Precedence" $ do
    it "Precedence test 1" $ "1 + 2 * 3" `shouldParse` DirectCall
      "+"
      [  a $ NumLiteral 1
      ,  a $ DirectCall "*" [ a $ NumLiteral 2, a $ NumLiteral 3]]
    it "Precedence test 2" $ "(1 + 2) * 3" `shouldParse` DirectCall
      "*"
      [ a $ DirectCall "+" [ a $ NumLiteral 1, a $ NumLiteral 2]
      , a $ NumLiteral 3]
    it "Precedence test 3" $ "(1 + -2) * 3" `shouldParse` DirectCall
      "*"
      [ a $ DirectCall "+" [ a $ NumLiteral 1, a $ DirectCall "-" [a $ NumLiteral 2]]
      , a $ NumLiteral 3]
 where
  shouldParse inp ast =
    testParse ("module Test where function func(): nil {return " ++  inp ++ ";}")
      `shouldBe` Right
                   (Module
                     [u "Test"]
                     []
                     [a $ FunctionDecl
                                    (l "func")
                                    (a $ simpleType Nil)
                                    []
                                    (a $ CodeBlock [a $ Return $ Just $ a ast])
                     ]
                   )

spec = do
  describe "Module declaration" moduleSpec
  describe "Import declaration" importSpec
  describe "Declarations"       declarationSpec
  describe "Statements"         statementSpec
  describe "Expressions"        expressionSpec
