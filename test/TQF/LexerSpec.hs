module TQF.LexerSpec
  ( spec
  ) where

import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Lexer
import           Test.Hspec
import           Data.List.NonEmpty (NonEmpty((:|)))

unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = loop id
    where
        loop f = do
            x <- m
            if p x
                then loop (f . (x:))
                else return (f [])

shouldLex :: String -> [Token] -> Expectation
shouldLex inp expectedTokens = either error id (runAlex inp (unfoldWhileM (/=TokenEOF) alexMonadScan)) `shouldBe` expectedTokens

shouldNotLex :: String -> [Token] -> Expectation
shouldNotLex input expectedTokens = either error id (runAlex input (unfoldWhileM (/=TokenEOF) alexMonadScan)) `shouldNotBe` expectedTokens

u :: String -> Token
u = TokenIdentUpper . toIdent
l :: String -> Token
l = TokenIdentLower . toIdent

reservedWords =
  [ ("module"   , TokenModule)
  , ("where"    , TokenWhere)
  , ("import"   , TokenImport)
  , ("qualified", TokenQualified)
  , ("as"       , TokenAs)
  , ("if"       , TokenIf)
  , ("else"     , TokenElse)
  , ("while"    , TokenWhile)
  , ("return"   , TokenReturn)
  , ("false"    , TokenBool False)
  , ("true"     , TokenBool True)
  ]

symbols =
  [ ("=" , TokenAssign)
  , ("(" , TokenOpenP)
  , (")" , TokenCloseP)
  , ("[" , TokenOpenSquare)
  , ("]" , TokenCloseSquare)
  , ("{" , TokenOpenBrace)
  , ("}" , TokenCloseBrace)
  , ("<" , TokenOpenArrow)
  , (">" , TokenCloseArrow)
  , (";" , TokenSemicolon)
  , ("," , TokenComma)
  , ("+" , TokenAdd)
  , ("-" , TokenSub)
  , ("*" , TokenMul)
  , ("/" , TokenDiv)
  , ("%" , TokenMod)
  , ("&&", TokenAnd)
  , ("||", TokenOr)
  , ("==", TokenEq)
  , ("<=", TokenLe)
  , (">=", TokenGe)
  , ("!=", TokenNe)
  , ("!" , TokenNot)
  ]

spec = do
  describe "Comments and spaces" $ do
    it "Should skip inline comments" $ "--testComment" `shouldLex` []
    it "Should continue inline comments after the end of line"
      $           "-- test comment\nnextLine"
      `shouldLex` [l "nextLine"]
    it "Should skip any whitespace"
      $           "    aroundSpaces \t  test"
      `shouldLex` [l "aroundSpaces", l "test"]
  describe "LowerIdent" $ do
    it "Should lex an ident for any text that starts with a lowercase" $ do
      "ident" `shouldLex` [l "ident"]
      "Ident" `shouldLex` [u "Ident"]
    it "Should not lex reserved words as an ident" $ do
      "while" `shouldNotLex` [l "while"]
    it "Should correctly lex idents that contain reserved words" $ do
      "iwhile" `shouldLex` [l "iwhile"]
      "whilei" `shouldLex` [l "whilei"]
      "iwhilei" `shouldLex` [l "iwhilei"]
    it "Should correctly lex idents with namespaces" $ do
      "Test.Module.ident" `shouldLex` [l "Test.Module.ident"]
  describe "UpperIdent" $ do
    it "Should lex an ident for any text that starts with a uppercase" $ do
      "Ident" `shouldLex` [u "Ident"]
      "ident" `shouldNotLex` [u "Ident"]
    it "Should correctly lex idents that contain reserved words" $ do
      "Iwhile" `shouldLex` [u "Iwhile"]
      "Whilei" `shouldLex` [u "Whilei"]
      "Iwhilei" `shouldLex` [u "Iwhilei"]
    it "Should correctly lex idents with namespaces" $ do
      "Test.Module.Ident" `shouldLex` [u "Test.Module.Ident"]
  describe "Reserved words" $ do
    it "Should lex all reserved words"
      $ mapM_ (\(ident, token) -> ident `shouldLex` [token]) reservedWords
  describe "Symbols" $ do
    it "Should lex all symbols" $ mapM_ (\(ident, token) -> ident `shouldLex` [token]) symbols
  describe "Literals" $ do
    describe "Bool" $ do
      it "Should lex true" $ "true" `shouldLex` [TokenBool True]
      it "Should lex false" $ "false" `shouldLex` [TokenBool False]
    describe "Num" $ do
      it "Should lex natural numbers" $ do
        "1" `shouldLex` [TokenNum 1]
        "2342" `shouldLex` [TokenNum 2342]
        "0" `shouldLex` [TokenNum 0]
      it "Should lex negative numbers" $ do
        "-2" `shouldLex` [TokenSub, TokenNum 2]
      it "Should lex decimal numbers" $ do
        "2.4" `shouldLex` [TokenNum 2.4]
      it "Should lex hex numbers" $ do
        "0x24af" `shouldLex` [TokenNum 9391]
    describe "String" $ do
      it "Should lex the empty string" $ "\"\"" `shouldLex` [TokenString ""]
      it "Should lex a string with normal text inside"
        $           "\"test 123 . <= while\""
        `shouldLex` [TokenString "test 123 . <= while"]
      it "Should lex a string with escaped characters inside"
        $           "\"test \\t \\n \\\"\""
        `shouldLex` [TokenString "test \\t \\n \\\""]
