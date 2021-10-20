{
module TQF.Lexer
  ( Token(..)
  , unLex
  , alexScanTokens
  ) where

import Prelude hiding (lex)
import Control.Monad (liftM)
import TQF.AST
import Data.List.Split
}

%wrapper "basic"

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
  "extern" 	{ constToken TokenExtern }
  "if"		{ constToken TokenIf }
  "else"		{ constToken TokenElse }
  "while"		{ constToken TokenWhile }
  "do"		{ constToken TokenDo }
  "return"	{ constToken TokenReturn }
-- Reserved Symbols
  "="		{ constToken TokenAssign }
-- Groupings
  \(		{ constToken TokenOpenP }
  \)		{ constToken TokenCloseP }
  \[		{ constToken TokenOpenSquare }
  \]		{ constToken TokenCloseSquare }
  "{"		{ constToken TokenOpenBrace }
  "}"		{ constToken TokenCloseBrace }
  "<"		{ constToken TokenOpenArrow }
  ">"		{ constToken TokenCloseArrow }
-- Delimeters
  ";"		{ constToken TokenSemicolon }
  ","		{ constToken TokenComma }
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
-- Literals
  "true"								{ constToken (TokenBool True) }
  "false"								{ constToken (TokenBool False) }
  $digit+ { tok (TokenNum) }
  $digit+\.$digit+ { tok (TokenNum) }
  \.$digit+ { tok (TokenNum) }
  "0x"[0-9a-f]+						{ tok (TokenNum) }
  \"@string*\" { tok (TokenString) }
-- Identifiers
  (@uident ".")* @lident 		{ tok (identTok TokenIdentLower Var VarName) }
  (@uident ".")* @uident 		{ tok (identTok TokenIdentUpper Type TypeName) }
{

identTok :: (ident -> token) -> (ResolveableModule -> name -> ident) -> (String -> name) -> String -> token
identTok toToken toIdent toName input = toToken $ toIdent namespace ident
  where
    namespace = TypeName <$> init splitItems
    ident = toName $ last splitItems
    splitItems = splitOn "." input

data Token
  = TokenModule
  | TokenWhere
  | TokenImport
  | TokenQualified
  | TokenAs
  | TokenExtern
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenDo
  | TokenReturn
  | TokenAssign
  | TokenOpenP
  | TokenCloseP
  | TokenOpenSquare
  | TokenCloseSquare
  | TokenOpenBrace
  | TokenCloseBrace
  | TokenOpenArrow
  | TokenCloseArrow
  | TokenSemicolon
  | TokenComma
  | TokenDot
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
  | TokenBool Bool
  | TokenNum String
  | TokenString String
  | TokenIdentLower Var
  | TokenIdentUpper Type
  | TokenEOF
  deriving ( Show, Eq )

tok = ($)
constToken = const

-- For nice parser error messages.
unLex :: Token -> String
unLex TokenModule = "module"
unLex TokenWhere = "where"
unLex TokenImport = "import"
unLex TokenQualified = "qualified"
unLex TokenAs = "as"
unLex TokenExtern = "extern"
unLex TokenIf = "if"
unLex TokenElse = "else"
unLex TokenWhile = "while"
unLex TokenDo = "do"
unLex TokenReturn = "return"
unLex TokenAssign = "="
unLex TokenOpenP = "("
unLex TokenCloseP = ")"
unLex TokenOpenSquare = "["
unLex TokenCloseSquare = "]"
unLex TokenOpenBrace = "{"
unLex TokenCloseBrace = "}"
unLex TokenOpenArrow = "<"
unLex TokenCloseArrow = ">"
unLex TokenSemicolon = ";"
unLex TokenComma = ","
unLex TokenDot = "."
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
unLex TokenNot = "!"
unLex (TokenBool True) = "true"
unLex (TokenBool False) = "false"
unLex (TokenNum x) = x
unLex (TokenString x) = x
unLex (TokenIdentLower (Var modules ident)) = unLexModules modules ++ unVarName ident
unLex (TokenIdentUpper (Type modules ident)) = unLexModules modules ++ unTypeName ident
unLex TokenEOF = "<EOF>"

unLexModules :: ResolveableModule -> String
unLexModules = concat . map ((++".") . unTypeName)
}