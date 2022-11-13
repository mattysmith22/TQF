module TQF.ParserSpec
  ( spec
  ) where

import           Helpers
import           TQF.AST
import           TQF.Lexer
import           TQF.Parser
import           TQF.Type
import           Test.Hspec

shouldParse' :: String -> Module Parsed -> Expectation
shouldParse' input ast = parse (alexScanTokens input) `shouldBe` Right ast

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
  shouldParse inp ast = parse (alexScanTokens "module Test where" ++ alexScanTokens inp)
    `shouldBe` Right (Module [u "Test"] ast [])

declarationSpec = do
  it "Should parse a function declaration" $ do
    "function functionName(nil input): string {}"
      `shouldParse` [ FunctionDecl (l "functionName")
                                   (simpleType String)
                                   [(simpleType Nil, l "input")]
                                   (CodeBlock [])
                    ]
  it "Should parse a function declaration with namespaced types" $ do
    "function functionName(NS.Void input): NS.String {}"
      `shouldParse` [ FunctionDecl (l "functionName")
                                   (extra $ UIdent [u "NS"] (u "String"))
                                   [(extra $ UIdent [u "NS"] (u "Void"), l "input")]
                                   (CodeBlock [])
                    ]
  it "Should parse a function declaration with multiple arguments" $ do
    "function functionName(nil input, num input2): string {}"
      `shouldParse` [ FunctionDecl
                        (l "functionName")
                        (simpleType String)
                        [(simpleType Nil, l "input"), (simpleType Number, l "input2")]
                        (CodeBlock [])
                    ]
  it "Should parse multiple function declarations"
    $             "function functionName(nil input): string {} function functionName2(string input2): num {}"
    `shouldParse` [ FunctionDecl (l "functionName")
                                 (simpleType String)
                                 [(simpleType Nil, l "input")]
                                 (CodeBlock [])
                  , FunctionDecl (l "functionName2")
                                 (simpleType Number)
                                 [(simpleType String, l "input2")]
                                 (CodeBlock [])
                  ]
  it "Should parse an external function declarations"
    $ "external function functionName(nil input, num input2): bool = \"test\""
    `shouldParse` [ ExternalFunctionDecl
        (l "functionName")
        (simpleType Bool)
        [(simpleType Nil, l "input"), (simpleType Number, l "input2")]
        "test"
        ]
  it "Should parse an external variable declaration"
    $ "external varName: string = \"test\""
    `shouldParse` [ ExternalVariableDecl
        (l "varName")
        (simpleType String)
        "test"
        ]
 where
  shouldParse inp ast = parse (alexScanTokens "module Test where" ++ alexScanTokens inp)
    `shouldBe` Right (Module [u "Test"] [] ast)

statementSpec = do
  describe "FunctionDecl call" $ do
    it "Should parse a normal function call"
      $             "func(1,\"test\");"
      `shouldParse` [FunctionCall (toIdent "func") [NumLiteral 1, StringLiteral "test"]]
    it "Should parse an empty function call"
      $             "func();"
      `shouldParse` [FunctionCall (toIdent "func") []]
    it "Should parse a function call in another namespace"
      $             "Test.func(1);"
      `shouldParse` [FunctionCall (toIdent "Test.func") [NumLiteral 1]]
  describe "Return" $ do
    it "Should parse a normal return statement"
      $             "return 1;"
      `shouldParse` [Return $ Just $ NumLiteral 1]
    it "Should parse a return statement without a value" $ "return;" `shouldParse` [Return Nothing]
  describe "Code Block" $ do
    it "Should parse a normal empty code block statement" $ "{}" `shouldParse` [CodeBlock []]
    it "Should parse a normal code block with a single internal statement"
      $             "{return 1;}"
      `shouldParse` [CodeBlock [Return $ Just $ NumLiteral 1]]
    it "Should parse a normal code block with multiple internal statements"
      $             "{return 1; return 2;}"
      `shouldParse` [CodeBlock [Return $ Just $ NumLiteral 1, Return $ Just $ NumLiteral 2]]
  describe "Variable Declarations" $ do
    it "Should parse a variable declaration without an assignment"
      $             "varName: Type;"
      `shouldParse` [VariableDeclaration (extra $ typN' [] "Type") (l "varName") Nothing]
    it "Should parse a variable declaration with an assignment"
      $             "varName: Type = 1;"
      `shouldParse` [VariableDeclaration (extra $ typN' [] "Type") (l "varName") (Just $ NumLiteral 1)]
  describe "Assignment"
    $             it "Should parse an assignment"
    $             "varName = 1;"
    `shouldParse` [Assignment (varN' [] "varName") (NumLiteral 1)]
  describe "If Statement" $ do
    it "Should parse a normal if statement"
      $             "if (true) {return 1;}"
      `shouldParse` [ IfStatement (BoolLiteral True)
                                  (CodeBlock [Return $ Just $ NumLiteral 1])
                                  Nothing
                    ]
    it "Should parse an if statement with an else statement"
      $             "if (true) {return 1;} else {return 2;}"
      `shouldParse` [ IfStatement (BoolLiteral True) (CodeBlock [Return $ Just $ NumLiteral 1])
                        $ Just (CodeBlock [Return $ Just $ NumLiteral 2])
                    ]
    it "Should parse an if statement that doesn't use code blocks" $ do
      "if (true) return 1 else return 2;"
        `shouldParse` [ IfStatement (BoolLiteral True)
                                    (Return $ Just $ NumLiteral 1)
                                    (Just $ Return $ Just $ NumLiteral 2)
                      ]
      "if (true) return 1;"
        `shouldParse` [IfStatement (BoolLiteral True) (Return $ Just $ NumLiteral 1) Nothing]
  describe "While statement" $ do
    it "Should parse a normal while loop"
      $             "while (true) {return 1;}"
      `shouldParse` [WhileLoop (BoolLiteral True) (CodeBlock [Return $ Just $ NumLiteral 1])]
    it "Should parse a while loop that doesn't use code blocks"
      $             "while (true) return 1;"
      `shouldParse` [WhileLoop (BoolLiteral True) (Return $ Just $ NumLiteral 1)]
 where
  shouldParse inp ast =
    parse
        (alexScanTokens "module Test where function func(): nil {" ++ alexScanTokens inp ++ [TokenCloseBrace]
        )
      `shouldBe` Right
                   (Module [u "Test"]
                           []
                           [FunctionDecl (l "func") (simpleType Nil) [] (CodeBlock ast)]
                   )

expressionSpec = do
  describe "Variables" $ do
    it "Should parse a normal variable reading" $ "variable" `shouldParse` Variable
      (varN' [] "variable")
    it "Should parse a variable reading and Namespaces"
      $             "Namespace.variable"
      `shouldParse` Variable (varN' ["Namespace"] "variable")
  describe "Literals" $ do
    it "Should parse bool literals" $ do
      "true" `shouldParse` BoolLiteral True
      "false" `shouldParse` BoolLiteral False
    it "Should parse normal integer literals" $ do
      "15" `shouldParse` NumLiteral 15
      "0" `shouldParse` NumLiteral 0
      "-15" `shouldParse` NumLiteral (-15)
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
      "-(20)" `shouldParse` DirectCall "-" [NumLiteral 20]
    it "Should parse not operators" $ do
      "!true" `shouldParse` DirectCall "!" [BoolLiteral True]
  describe "Binary Operators" $ do
    it "Should parse mathematical operators" $ do
      "1+2" `shouldParse` DirectCall "+" [NumLiteral 1, NumLiteral 2]
      "1-2" `shouldParse` DirectCall "-" [NumLiteral 1, NumLiteral 2]
      "1*2" `shouldParse` DirectCall "*" [NumLiteral 1, NumLiteral 2]
      "1/2" `shouldParse` DirectCall "/" [NumLiteral 1, NumLiteral 2]
      "1%2" `shouldParse` DirectCall "%" [NumLiteral 1, NumLiteral 2]
    it "Should parse boolean operators" $ do
      "1&&2" `shouldParse` DirectCall "&&" [NumLiteral 1, NumLiteral 2]
      "1||2" `shouldParse` DirectCall "||" [NumLiteral 1, NumLiteral 2]
    it "Should parse comparison operators" $ do
      "1==2" `shouldParse` DirectCall "isEqualTo" [NumLiteral 1, NumLiteral 2]
      "1!=2" `shouldParse` DirectCall "isNotEqualTo" [NumLiteral 1, NumLiteral 2]
      "1<2" `shouldParse` DirectCall "<" [NumLiteral 1, NumLiteral 2]
      "1>2" `shouldParse` DirectCall ">" [NumLiteral 1, NumLiteral 2]
      "1>=2" `shouldParse` DirectCall ">=" [NumLiteral 1, NumLiteral 2]
      "1<=2" `shouldParse` DirectCall "<=" [NumLiteral 1, NumLiteral 2]
  describe "FunctionDecl calls" $ do
    it "Should parse a function call" $ "func(1, 2)" `shouldParse` FuncCall
      (varN' [] "func")
      [NumLiteral 1, NumLiteral 2]
  describe "Array construction" $ do
    it "Should parse an empty array" $ "[]" `shouldParse` ArrayExpr []
    it "Should parse a single array" $ "[1]" `shouldParse` ArrayExpr [NumLiteral 1]
    it "Should parse multiple elements of an array" $ "[1, 2]" `shouldParse` ArrayExpr
      [NumLiteral 1, NumLiteral 2]
  describe "Direct calls" $ do
    it "Should parse a nular direct call" $ "<sqfCommand>()" `shouldParse` DirectCall "sqfCommand" []
    it "Should parse a unary direct call" $ "<sqfCommand>(1)" `shouldParse` DirectCall
      "sqfCommand"
      [NumLiteral 1]
    it "Should parse a binary direct call" $ "<sqfCommand>(1,2)" `shouldParse` DirectCall
      "sqfCommand"
      [NumLiteral 1, NumLiteral 2]
  describe "Type cast" $ it "Should parse a type cast" $ "(NS.Type)1" `shouldParse` Cast
    (extra $ typN' ["NS"] "Type")
    (NumLiteral 1)
  describe "Precedence" $ do
    it "Precedence test 1" $ "1 + 2 * 3" `shouldParse` DirectCall
      "+"
      [ NumLiteral 1
      , DirectCall "*" [NumLiteral 2, NumLiteral 3]]
    it "Precedence test 2" $ "(1 + 2) * 3" `shouldParse` DirectCall
      "*"
      [ DirectCall "+" [NumLiteral 1, NumLiteral 2]
      , NumLiteral 3]
    it "Precedence test 3" $ "(1 + -2) * 3" `shouldParse` DirectCall
      "*"
      [ DirectCall "+" [NumLiteral 1,NumLiteral (-2)]
      , NumLiteral 3]
 where
  shouldParse inp ast =
    parse
        (  alexScanTokens "module Test where function func(): nil {return "
        ++ alexScanTokens inp
        ++ [TokenSemicolon, TokenCloseBrace]
        )
      `shouldBe` Right
                   (Module
                     [u "Test"]
                     []
                     [ FunctionDecl
                                    (l "func")
                                    (simpleType Nil)
                                    []
                                    (CodeBlock [Return $ Just ast])
                     ]
                   )

spec = do
  describe "Module declaration" moduleSpec
  describe "Import declaration" importSpec
  describe "Declarations"       declarationSpec
  describe "Statements"         statementSpec
  describe "Expressions"        expressionSpec
