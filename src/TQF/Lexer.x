{
module TQF.Lexer
  ( Token(..)
  , Alex(..)
  , unLex
  , alexEOF
  , alexMonadScan
  , AlexPosn(..)
  , alexMonadScanAnnot
  , alexError
  , alexGetInput
  , runAlex
  ) where

import Prelude hiding (lex)
import Control.Monad (liftM)
import TQF.AST
import TQF.AST.Annotated
import qualified TQF.Type as Type
import Data.List.Split
import Data.Char (isLower)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (break)
}

%wrapper "monad"

@escape = \\$printable
@string = [^\"\\] | @escape


$lower = [a-z]
$upper = [A-Z]
$digit = [0-9]
$identRemain = [a-zA-Z0-9]

@uident = $upper $identRemain*
@lident = $lower $identRemain*
tokens :-
-- Basic emitters
  $white+		;
  "--"[^\n]*		;
-- Reserved words
  "module"	{ constToken TokenModule }
  "where"		{ constToken TokenWhere }
  "import"	{ constToken TokenImport }
  "qualified"	{ constToken TokenQualified }
  "as"		{ constToken TokenAs }
  "if"		{ constToken TokenIf }
  "else"		{ constToken TokenElse }
  "while"		{ constToken TokenWhile }
  "return"	{ constToken TokenReturn }
  "type" { constToken TokenType }
  "function" { constToken TokenFunction }
  "global" { constToken TokenGlobal}
  "command" { constToken TokenCommand }
  "external" { constToken TokenExternal }
-- Types
  "top" { constToken $ TokenTop }
  "string" { constToken $ TokenSimpleType  Type.String}
  "num" { constToken $ TokenSimpleType  Type.Number}
  "array" { constToken $ TokenSimpleType  Type.Array}
  "hashmap" { constToken $ TokenSimpleType  Type.HashMap}
  "bool" { constToken $ TokenSimpleType  Type.Bool}
  "code" { constToken $ TokenSimpleType  Type.Code}
  "nil" { constToken $ TokenSimpleType  Type.Nil}
-- Reserved Symbols
  "="		{ constToken TokenAssign }
-- Groupings
  \(		{ constToken TokenOpenP }
  \)		{ constToken TokenCloseP }
  "'("  { constToken TokenOpenPTuple }
  \[		{ constToken TokenOpenSquare }
  \]		{ constToken TokenCloseSquare }
  "{"		{ constToken TokenOpenBrace }
  "}"		{ constToken TokenCloseBrace }
  "<"		{ constToken TokenOpenArrow }
  ">"		{ constToken TokenCloseArrow }
-- Delimeters
  ";"		{ constToken TokenSemicolon }
  ","		{ constToken TokenComma }
-- Literals
  "true"								{ constToken (TokenBool True) }
  "false"								{ constToken (TokenBool False) }
  $digit+ { tok (TokenNum . read) }
  $digit+\.$digit+ { tok (TokenNum . read) }
  "0x"[0-9a-f]+						{ tok (TokenNum . read) }
  \"@string*\" { tok (TokenString . init . tail) }
-- Operators
  "+"		{ constToken TokenAdd }
  "-"		{ constToken TokenSub }
  "*"		{ constToken TokenMul }
  "/"		{ constToken TokenDiv }
  "%"		{ constToken TokenMod }
  "&&"	{ constToken TokenAnd }
  "||"	{ constToken TokenOr }
  "=="	{ constToken TokenEq }
  "<="	{ constToken TokenLe }
  ">="	{ constToken TokenGe }
  "!="	{ constToken TokenNe }
  "!"		{ constToken TokenNot }
  "|"   { constToken TokenPipe }
  ":"   { constToken TokenColon }
-- Identifiers
  (@uident ".")*@lident("."@lident)* 		{ tok (TokenIdentLower . toIdent) }
  (@uident ".")* @uident 		{ tok (TokenIdentUpper . toIdent) }
{

data Token
  = TokenModule
  | TokenWhere
  | TokenImport
  | TokenQualified
  | TokenAs
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenReturn
  | TokenType
  | TokenFunction
  | TokenGlobal
  | TokenCommand
  | TokenExternal
  | TokenAssign
  | TokenTop
  | TokenOpenP
  | TokenOpenPTuple
  | TokenCloseP
  | TokenOpenSquare
  | TokenCloseSquare
  | TokenOpenBrace
  | TokenCloseBrace
  | TokenOpenArrow
  | TokenCloseArrow
  | TokenSemicolon
  | TokenComma
  | TokenPipe
  | TokenColon
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenMod
  | TokenAnd
  | TokenOr
  | TokenEq
  | TokenLe
  | TokenGe
  | TokenNe
  | TokenNot
  | TokenSimpleType Type.SimpleType
  | TokenBool Bool
  | TokenNum Double
  | TokenString String
  | TokenIdentLower LIdent
  | TokenIdentUpper UIdent
  | TokenEOF
  deriving ( Show, Eq )

-- For nice parser error messages.
unLex :: Token -> String
unLex TokenModule = "module"
unLex TokenWhere = "where"
unLex TokenImport = "import"
unLex TokenQualified = "qualified"
unLex TokenAs = "as"
unLex TokenIf = "if"
unLex TokenElse = "else"
unLex TokenWhile = "while"
unLex TokenReturn = "return"
unLex TokenType = "type"
unLex TokenFunction = "function"
unLex TokenGlobal = "global"
unLex TokenCommand = "command"
unLex TokenExternal = "external"
unLex TokenAssign = "="
unLex TokenOpenP = "("
unLex TokenOpenPTuple = "'("
unLex TokenCloseP = ")"
unLex TokenOpenSquare = "["
unLex TokenCloseSquare = "]"
unLex TokenOpenBrace = "{"
unLex TokenCloseBrace = "}"
unLex TokenOpenArrow = "<"
unLex TokenCloseArrow = ">"
unLex TokenSemicolon = ";"
unLex TokenComma = ","
unLex TokenAdd = "+"
unLex TokenSub = "-"
unLex TokenMul = "*"
unLex TokenDiv = "/"
unLex TokenMod = "%"
unLex TokenAnd = "&&"
unLex TokenOr = "||"
unLex TokenEq = "=="
unLex TokenLe = "<="
unLex TokenGe = ">="
unLex TokenNe = "!="
unLex TokenColon = ":"
unLex TokenPipe = "|"
unLex TokenNot = "!"
unLex (TokenBool True) = "true"
unLex (TokenBool False) = "false"
unLex (TokenNum x) = show x
unLex (TokenString x) = x
unLex (TokenSimpleType x) = Type.simpleTypeToString x
unLex (TokenIdentLower (LIdent modules (ident:|rest))) = unLexModules modules ++ unVarName ident ++ concatMap (("."++) . unVarName) rest
unLex (TokenIdentUpper (UIdent modules ident)) = unLexModules modules ++ unTypeName ident
unLex TokenEOF = "<EOF>"

unLexModules :: ResolveableModule -> String
unLexModules = concat . map ((++".") . unTypeName)

alexEOF :: Alex Token
alexEOF = return TokenEOF

-- | Custom scan function which annotates every lexed token with the range in which it occurs within the test
alexMonadScanAnnot :: Alex (Annot Token)
alexMonadScanAnnot = do
  inp__ <- alexGetInput
  let (AlexPn _ lineStart colStart,_,_,_) = inp__
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> (Annot (Range (Pos lineStart colStart) (Pos lineStart colStart))) <$> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScanAnnot
    (AlexToken endInp len action) -> do
        let (AlexPn _ lineEnd colEnd,_,_,_) = endInp
        alexSetInput endInp
        let range = Range (Pos lineStart colStart) (Pos lineEnd colEnd)
        Annot range <$> action (ignorePendingBytes inp__) len


tok :: (String -> Token) -> AlexAction Token
tok f (_,_,_,inp) n = pure $ f (take n inp)
constToken :: Token -> AlexAction Token
constToken x _ _ = pure x
}