module TQF.ParserSpec
  ( spec
  ) where

import           TQF.AST
import           TQF.Lexer
import           TQF.Parser
import           Test.Hspec

shouldParse' :: String -> Module -> Expectation
shouldParse' input ast = parse (alexScanTokens input) `shouldBe` Right ast

u :: String -> TypeName
u = TypeName
l :: String -> VarName
l = VarName


typeVal :: [String] -> String -> Type
typeVal modules = Type (TypeName <$> modules) . TypeName
varVal :: [String] -> String -> Var
varVal modules = Var (TypeName <$> modules) . VarName

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
    "String functionName(Void input) {}"
      `shouldParse` [ FunctionDecl []
                               (l "functionName")
                               (Type [] (u "String"))
                               []
                               [(Type [] (u "Void"), l "input")]
                               (CodeBlock [])
                    ]
  it "Should parse a function declaration with namespaced types" $ do
    "NS.String functionName(NS.Void input) {}"
      `shouldParse` [ FunctionDecl []
                               (l "functionName")
                               (Type [u "NS"] (u "String"))
                               []
                               [(Type [u "NS"] (u "Void"), l "input")]
                               (CodeBlock [])
                    ]
  it "Should parse a function declaration with closures" $ do
    "String functionName[Num closure](Void input) {}"
      `shouldParse` [ FunctionDecl []
                               (l "functionName")
                               (Type [] (u "String"))
                               [(Type [] (u "Num"), l "closure")]
                               [(Type [] (u "Void"), l "input")]
                               (CodeBlock [])
                    ]
  it "Should parse a function declaration with multiple arguments" $ do
    "String functionName(Void input, Num input2) {}"
      `shouldParse` [ FunctionDecl []
                               (l "functionName")
                               (Type [] (u "String"))
                               []
                               [(Type [] (u "Void"), l "input"), (Type [] (u "Num"), l "input2")]
                               (CodeBlock [])
                    ]
  it "Should parse multiple function declarations"
    $             "String functionName(Void input) {} Num functionName2(Object input2) {}"
    `shouldParse` [ FunctionDecl []
                             (l "functionName")
                             (Type [] (u "String"))
                             []
                             [(Type [] (u "Void"), l "input")]
                             (CodeBlock [])
                  , FunctionDecl []
                             (l "functionName2")
                             (Type [] (u "Num"))
                             []
                             [(Type [] (u "Object"), l "input2")]
                             (CodeBlock [])
                  ]
 where
  shouldParse inp ast = parse (alexScanTokens "module Test where" ++ alexScanTokens inp)
    `shouldBe` Right (Module [u "Test"] [] ast)

statementSpec = do
  describe "FunctionDecl call" $ do
    it "Should parse a normal function call"
      $             "func(1,\"test\");"
      `shouldParse` [FunctionCall (Var [] (l "func")) [NumLiteral "1", StringLiteral "\"test\""]]
    it "Should parse an empty function call"
      $             "func();"
      `shouldParse` [FunctionCall (Var [] (l "func")) []]
    it "Should parse a function call in another namespace"
      $             "Test.func(1);"
      `shouldParse` [FunctionCall (Var [u "Test"] (l "func")) [NumLiteral "1"]]
  describe "Return" $ do
    it "Should parse a normal return statement"
      $             "return 1;"
      `shouldParse` [Return $ Just $ NumLiteral "1"]
    it "Should parse a return statement without a value" $ "return;" `shouldParse` [Return Nothing]
  describe "Code Block" $ do
    it "Should parse a normal empty code block statement" $ "{}" `shouldParse` [CodeBlock []]
    it "Should parse a normal code block with a single internal statement"
      $             "{return 1;}"
      `shouldParse` [CodeBlock [Return $ Just $ NumLiteral "1"]]
    it "Should parse a normal code block with multiple internal statements"
      $             "{return 1; return 2;}"
      `shouldParse` [CodeBlock [Return $ Just $ NumLiteral "1", Return $ Just $ NumLiteral "2"]]
  describe "Variable Declarations" $ do
    it "Should parse a variable declaration without an assignment"
      $             "Type varName;"
      `shouldParse` [VariableDeclaration (typeVal [] "Type") (l "varName") Nothing]
    it "Should parse a variable declaration with an assignment"
      $             "Type varName = 1;"
      `shouldParse` [VariableDeclaration (typeVal [] "Type") (l "varName") (Just $ NumLiteral "1")]
  describe "Assignment"
    $             it "Should parse an assignment"
    $             "varName = 1;"
    `shouldParse` [Assignment (varVal [] "varName") (NumLiteral "1")]
  describe "If Statement" $ do
    it "Should parse a normal if statement"
      $             "if (true) {return 1;}"
      `shouldParse` [ IfStatement (BoolLiteral True)
                                  (CodeBlock [Return $ Just $ NumLiteral "1"])
                                  Nothing
                    ]
    it "Should parse an if statement with an else statement"
      $             "if (true) {return 1;} else {return 2;}"
      `shouldParse` [ IfStatement (BoolLiteral True) (CodeBlock [Return $ Just $ NumLiteral "1"])
                        $ Just (CodeBlock [Return $ Just $ NumLiteral "2"])
                    ]
    it "Should parse an if statement that doesn't use code blocks" $ do
      "if (true) return 1 else return 2;"
        `shouldParse` [ IfStatement (BoolLiteral True)
                                    (Return $ Just $ NumLiteral "1")
                                    (Just $ Return $ Just $ NumLiteral "2")
                      ]
      "if (true) return 1;"
        `shouldParse` [IfStatement (BoolLiteral True) (Return $ Just $ NumLiteral "1") Nothing]
  describe "While statement" $ do
    it "Should parse a normal while loop"
      $             "while (true) {return 1;}"
      `shouldParse` [WhileLoop (BoolLiteral True) (CodeBlock [Return $ Just $ NumLiteral "1"])]
    it "Should parse a while loop that doesn't use code blocks"
      $             "while (true) return 1;"
      `shouldParse` [WhileLoop (BoolLiteral True) (Return $ Just $ NumLiteral "1")]
  describe "Do While Statement" $ do
    it "Should parse a normal Do While statement"
      $             "do {return 1;} while (true);"
      `shouldParse` [DoWhile (BoolLiteral True) (CodeBlock [Return $ Just $ NumLiteral "1"])]
 where
  shouldParse inp ast =
    parse
        (alexScanTokens "module Test where Void func() {" ++ alexScanTokens inp ++ [TokenCloseBrace]
        )
      `shouldBe` Right
                   (Module [u "Test"]
                           []
                           [FunctionDecl [] (l "func") (Type [] (u "Void")) [] [] (CodeBlock ast)]
                   )

expressionSpec = do
  describe "Variables" $ do
    it "Should parse a normal variable reading" $ "variable" `shouldParse` Variable
      (varVal [] "variable")
    it "Should parse a variable reading and Namespaces"
      $             "Namespace.variable"
      `shouldParse` Variable (varVal ["Namespace"] "variable")
  describe "Literals" $ do
    it "Should parse bool literals" $ do
      "true" `shouldParse` BoolLiteral True
      "false" `shouldParse` BoolLiteral False
    it "Should parse normal integer literals" $ do
      "15" `shouldParse` NumLiteral "15"
      "0" `shouldParse` NumLiteral "0"
      "-15" `shouldParse` UnaryOperator NegOp (NumLiteral "15")
    it "Should parse normal decimal literals" $ do
      ".15" `shouldParse` NumLiteral ".15"
      "0.15" `shouldParse` NumLiteral "0.15"
    it "Should parse normal hex literals" $ "0x1a5" `shouldParse` NumLiteral "0x1a5"
    it "Should parse string literals" $ do
      "\"Test\"" `shouldParse` StringLiteral "\"Test\""
      "\"String with Sp@ces\"" `shouldParse` StringLiteral "\"String with Sp@ces\""
      "\"String \\n with \\\" escape \\t codes\""
        `shouldParse` StringLiteral "\"String \\n with \\\" escape \\t codes\""
  describe "Unary Operators" $ do
    it "Should parse negate operators" $ do
      "-20" `shouldParse` UnaryOperator NegOp (NumLiteral "20")
    it "Should parse not operators" $ do
      "!true" `shouldParse` UnaryOperator NotOp (BoolLiteral True)
  describe "Binary Operators" $ do
    it "Should parse mathematical operators" $ do
      "1+2" `shouldParse` BinaryOperator AddOp (NumLiteral "1") (NumLiteral "2")
      "1-2" `shouldParse` BinaryOperator SubOp (NumLiteral "1") (NumLiteral "2")
      "1*2" `shouldParse` BinaryOperator MulOp (NumLiteral "1") (NumLiteral "2")
      "1/2" `shouldParse` BinaryOperator DivOp (NumLiteral "1") (NumLiteral "2")
      "1%2" `shouldParse` BinaryOperator ModOp (NumLiteral "1") (NumLiteral "2")
    it "Should parse boolean operators" $ do
      "1&&2" `shouldParse` BinaryOperator AndOp (NumLiteral "1") (NumLiteral "2")
      "1||2" `shouldParse` BinaryOperator OrOp (NumLiteral "1") (NumLiteral "2")
    it "Should parse comparison operators" $ do
      "1==2" `shouldParse` BinaryOperator EqOp (NumLiteral "1") (NumLiteral "2")
      "1!=2" `shouldParse` BinaryOperator NotEqOp (NumLiteral "1") (NumLiteral "2")
      "1<2" `shouldParse` BinaryOperator LessOp (NumLiteral "1") (NumLiteral "2")
      "1>2" `shouldParse` BinaryOperator GreaterOp (NumLiteral "1") (NumLiteral "2")
      "1>=2" `shouldParse` BinaryOperator GreaterEqualOp (NumLiteral "1") (NumLiteral "2")
      "1<=2" `shouldParse` BinaryOperator LessEqualOp (NumLiteral "1") (NumLiteral "2")
  describe "FunctionDecl calls" $ do
    it "Should parse a function call" $ "func(1, 2)" `shouldParse` FuncCall
      (varVal [] "func")
      [NumLiteral "1", NumLiteral "2"]
  describe "Array construction" $ do
    it "Should parse an empty array" $ "[]" `shouldParse` Array []
    it "Should parse a single array" $ "[1]" `shouldParse` Array [NumLiteral "1"]
    it "Should parse multiple elements of an array" $ "[1, 2]" `shouldParse` Array
      [NumLiteral "1", NumLiteral "2"]
  describe "Direct calls" $ do
    it "Should parse a nular direct call" $ "<command>()" `shouldParse` DirectCall "command" []
    it "Should parse a unary direct call" $ "<command>(1)" `shouldParse` DirectCall
      "command"
      [NumLiteral "1"]
    it "Should parse a binary direct call" $ "<command>(1,2)" `shouldParse` DirectCall
      "command"
      [NumLiteral "1", NumLiteral "2"]
  describe "Type cast" $ it "Should parse a type cast" $ "(NS.Type)1" `shouldParse` Cast
    (typeVal ["NS"] "Type")
    (NumLiteral "1")
  describe "Precedence" $ do
    it "Precedence test 1" $ "1 + 2 * 3" `shouldParse` BinaryOperator
      AddOp
      (NumLiteral "1")
      (BinaryOperator MulOp (NumLiteral "2") (NumLiteral "3"))
    it "Precedence test 2" $ "(1 + 2) * 3" `shouldParse` BinaryOperator
      MulOp
      (BinaryOperator AddOp (NumLiteral "1") (NumLiteral "2"))
      (NumLiteral "3")
    it "Precedence test 3" $ "(1 + -2) * 3" `shouldParse` BinaryOperator
      MulOp
      (BinaryOperator AddOp (NumLiteral "1") (UnaryOperator NegOp (NumLiteral "2")))
      (NumLiteral "3")
 where
  shouldParse inp ast =
    parse
        (  alexScanTokens "module Test where Void func() {return "
        ++ alexScanTokens inp
        ++ [TokenSemicolon, TokenCloseBrace]
        )
      `shouldBe` Right
                   (Module
                     [u "Test"]
                     []
                     [ FunctionDecl []
                                (l "func")
                                (Type [] (u "Void"))
                                []
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
