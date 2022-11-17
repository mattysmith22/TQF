{
{-# LANGUAGE TypeFamilies #-}
module TQF.Parser (parse) where

import TQF.Lexer
import TQF.AST
import TQF.AST.Annotated
import qualified TQF.Type as Type
import qualified Data.Map as Map
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import SQF.Commands
}

%name parse
%tokentype { Annot Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexer } { Annot _ TokenEOF }

%token
    module    { Annot _ TokenModule }
    where     { Annot _ TokenWhere }
    import    { Annot _ TokenImport }
    qualified { Annot _ TokenQualified }
    as        { Annot _ TokenAs }
    if        { Annot _ TokenIf }
    else      { Annot _ TokenElse }
    while     { Annot _ TokenWhile }
    return    { Annot _ TokenReturn }
    type      { Annot _ TokenType }
    function  { Annot _ TokenFunction }
    global    { Annot _ TokenGlobal }
    top       { Annot _ TokenTop }
    command   { Annot _ TokenCommand }
    external  { Annot _ TokenExternal }
    '='       { Annot _ TokenAssign }
    '('       { Annot _ TokenOpenP }
    '\'('     { Annot _ TokenOpenPTuple }
    ')'       { Annot _ TokenCloseP }
    '['       { Annot _ TokenOpenSquare }
    ']'       { Annot _ TokenCloseSquare }
    '{'       { Annot _ TokenOpenBrace }
    '}'       { Annot _ TokenCloseBrace }
    '<'       { Annot _ TokenOpenArrow }
    '>'       { Annot _ TokenCloseArrow }
    ';'       { Annot _ TokenSemicolon }
    ','       { Annot _ TokenComma }
    '+'       { Annot _ TokenAdd }
    '-'       { Annot _ TokenSub }
    '*'       { Annot _ TokenMul }
    '/'       { Annot _ TokenDiv }
    '%'       { Annot _ TokenMod }
    '&&'      { Annot _ TokenAnd }
    '||'      { Annot _ TokenOr }
    '=='      { Annot _ TokenEq }
    '<='      { Annot _ TokenLe }
    '>='      { Annot _ TokenGe }
    '!='      { Annot _ TokenNe }
    '!'       { Annot _ TokenNot }
    '|'       { Annot _ TokenPipe }
    ':'       { Annot _ TokenColon }
    bool { $$@(Annot _ (TokenBool _)) }
    int { $$@(Annot _ (TokenNum _)) }
    string { $$@(Annot _ (TokenString _)) }
    lidentSimple { $$@(Annot _ (TokenIdentLower (LIdent [] (_:|[]))))}
    lident { $$@(Annot _ (TokenIdentLower _)) }
    uidentSimple { $$@(Annot _ (TokenIdentUpper (UIdent [] _)))}
    uident { $$@(Annot _ (TokenIdentUpper _)) }
    simpletype { $$@(Annot _ (TokenSimpleType _)) }

%left '||'
%right '&&'

%left '==' '!=' 
%left '<=' '>=' '<' '>'
%left '+' '-'
%left '*' '/' '%'
%left NEG '!'

%%

Module :: { Module Parsed }
    : ModuleDeclaration Imports Declarations {Module $1 $2 (reverse $3) :: Module Parsed}

ModuleDeclaration :: { ResolveableModule }
    : module ModuleIdent where {unAnnot $2}

Imports :: { [ImportStatement] }
    : {- empty -} {[]}
    | Import Imports { $1 : $2 }

Import :: { ImportStatement }
    : import ImportQualification ModuleIdent ImportRenaming { ImportStatement (unAnnot $3) $2 $4}

ImportQualification :: { Bool }
    : {- empty -} {False}
    | qualified {True}

ImportRenaming :: { Maybe ResolveableModule }
    : {- empty -} {Nothing}
    | as ModuleIdent {Just $ unAnnot $2}

Declarations :: { [Declaration Parsed] }
    : {- empty -} {[]}
    | Declarations Declaration { $2 : $1 }

Type :: { Annot ASTType }
    : SimpleType { fmap Type.simpleType $1 }
    | Num { fmap Type.constNumber $1 }
    | String { fmap Type.constString $1 }
    | Bool { fmap Type.constBool $1 }
    | top { Annot (pos $1) Type.top }
    | Type '|' Type { Annot (pos $1 <> pos $3) (unAnnot $1 <> unAnnot $3) }
    | '(' Type ')' { Annot (pos $1 <> pos $3) (unAnnot $2) }
    | UIdent { fmap Type.extra $1 }

    -- Tuple definition
    | '(' ')' { Annot (pos $1 <> pos $2) $ Type.tuple [] }
    | '\'(' TupleElements ')' { Annot (pos $1 <> pos $3) $ Type.tuple $2 }
    | '(' Type ',' Type TupleElements ')' { Annot (pos $1 <> pos $6) $ Type.tuple ( unAnnot $2 : unAnnot $4 : $5 ) }

    -- Record definition
    | '{' TypeRecordFields '}' { Annot (pos $1 <> pos $3) $ Type.record $ Map.fromList $2 }
    
TupleElements :: { [ASTType] }
    : {- empty -} {[]}
    | Type { [unAnnot $1] }
    | Type ',' TupleElements { unAnnot $1 : $3 }

TypeRecordFields :: { [(String, ASTType)] }
    : {- empty -} {[]}
    | TypeRecordField {[$1]}
    | TypeRecordField ',' TypeRecordFields { $1:$3 }

TypeRecordField :: { (String, ASTType) }
    : LIdentSimple ':' Type { (unVarName $ unAnnot $1, unAnnot $3) }

Declaration :: { Declaration Parsed }
    : function LIdentSimple '(' FunctionDeclArguments ')' ':' Type CodeBlock { Annot (pos $1 <> pos $8) $ FunctionDecl (unAnnot $2) $7 $4 (fmap CodeBlock $8) }
    | type UIdentSimple '=' Type { Annot (pos $1 <> pos $4) $ TypeDecl (unAnnot $2) $4 }
    | global LIdentSimple ':' Type { Annot (pos $1 <> pos $4) $ VariableDecl $4 (unAnnot $2) }
    | command LIdentSimple '(' FunctionDeclArguments ')' ':' Type '=' String { Annot (pos $1 <> pos $9) $ CommandDecl (unAnnot $9) (unAnnot $2) $7 $4 }
    | external function LIdentSimple '(' FunctionDeclArguments ')' ':' Type '=' String { Annot (pos $1 <> pos $10) $ ExternalFunctionDecl (unAnnot $3) $8 $5 (unAnnot $10) }
    | external LIdentSimple ':' Type '=' String { Annot (pos $1 <> pos $6) $ ExternalVariableDecl (unAnnot $2) $4 (unAnnot $6) }

FunctionDeclArguments :: { [(Annot ASTType, VarName)] }
    : {- empty -} {[]}
    | FunctionDeclArgument {[$1]}
    | FunctionDeclArgument ',' FunctionDeclArguments {$1:$3}

FunctionDeclArgument :: { (Annot ASTType, VarName) }
    : LIdentSimple ':' Type {($3, unAnnot $1)}

CodeBlock :: { Annot [Statement Parsed] }
    : '{' Statements '}' {Annot (pos $1 <> pos $3) $2}

Statements :: { [Statement Parsed] }
    : {- empty -} {[]}
    | Statement {[$1]}
    | Statement Statements {$1:$2}

StatementNoEndSemicolon :: { Statement Parsed }
    : CodeBlock {fmap CodeBlock $1}
    | if '(' Expr ')' IfStatements {Annot (pos $1 <> pos $5) $ IfStatement $3 (fst $ unAnnot $5) (snd $ unAnnot $5)}
    | while '(' Expr ')' Statement {Annot (pos $1 <> pos $5) $ WhileLoop $3 $5}

IfStatements :: { Annot (Statement Parsed, Maybe (Statement Parsed)) }
    : StatementNoSemicolon else Statement {Annot (pos $1 <> pos $3) $ ($1, Just $3)}
    | Statement {Annot (pos $1) $ ($1, Nothing)}

Statement :: { Statement Parsed }
    : StatementNoEndSemicolon {$1}
    | StatementEndSemicolon ';' {$1}

StatementNoSemicolon :: { Statement Parsed }
    : StatementNoEndSemicolon {$1}
    | StatementEndSemicolon {$1}

StatementEndSemicolon :: { Statement Parsed }
    : LIdent LidentStatement { Annot (pos $1 <> pos $2) $ (unAnnot $2) $1}
    | LIdentSimple ':' Type VariableDeclarationAssignment {Annot (pos $1 <> pos $3) $ VariableDeclaration $3 (unAnnot $1) $4}
    | return ReturnValue { Annot (pos $1) $ Return $2}

ReturnValue :: { Maybe (Expr Parsed) }
    : {- empty -} {Nothing}
    | Expr {Just $1}

VariableDeclarationAssignment :: { Maybe (Expr Parsed) }
    : {- empty -} {Nothing}
    | '=' Expr {Just $2}

LidentStatement :: { Annot (Annot LIdent -> Statement_ Parsed) }
    : '=' Expr {Annot (pos $1 <> pos $2) $ (\s -> Assignment s $2)}
    | '(' ExprList ')' {Annot (pos $1 <> pos $3) $ (\s -> FunctionCall s $2)}

ExprList :: { [Expr Parsed] }
    : {- empty -} {[]}
    | Expr {[$1]}
    | Expr ',' ExprList {$1:$3}

Expr :: { Expr Parsed }
    : Expr '+' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) AddOp) $1 $3}
    | Expr '-' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) SubOp) $1 $3}
    | Expr '*' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) MulOp) $1 $3}
    | Expr '/' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) DivOp) $1 $3}
    | Expr '%' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) ModOp) $1 $3}
    | Expr '&&' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) AndOp) $1 $3}
    | Expr '||' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) OrOp) $1 $3}
    | Expr '==' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) EqOp) $1 $3}
    | Expr '!=' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) NotEqOp) $1 $3}
    | Expr '<=' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) LessEqualOp) $1 $3}
    | Expr '>=' Expr {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) GreaterEqualOp) $1 $3}
    | Expr '<' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) LessOp) $1 $3}
    | Expr '>' Expr  {Annot (pos $1 <> pos $3) $ BinOp (Annot (pos $2) GreaterOp) $1 $3}
    | '!' Expr {Annot (pos $1 <> pos $2) $ UnOp (Annot (pos $1) NotOp) $2}
    | '-' Expr %prec NEG {Annot (pos $1 <> pos $2) $ UnOp (Annot (pos $1) NegOp) $2}

    | LIdent {Annot (pos $1) $ Variable $1}
    | LIdent '(' ExprList ')' {Annot (pos $1 <> pos $4) $ FuncCall $1 $3}
    | Bool {fmap BoolLiteral $1}
    | Num {fmap NumLiteral $1}
    | String {fmap StringLiteral $1}
    | '[' ExprList ']' {Annot (pos $1 <> pos $3) $ ArrayExpr $2}
    | '(' ExprList ')' { Annot (pos $1 <> pos $3) $ tupleOrParens $2}
    | '\'(' ExprList ')' { Annot (pos $1 <> pos $3) $ Tuple $2}
    | '<' Type '>' Expr {Annot (pos $1 <> pos $4) $ Cast $2 $4}

ModuleIdent :: { Annot ResolveableModule }
    : UIdent {fmap typeToModuleIdent $1}

Bool :: { Annot Bool }
    : bool { (\(Annot r (TokenBool x)) -> Annot r x) $1 }
Num :: { Annot Double }
    : int { (\(Annot r (TokenNum x)) -> Annot r x) $1 }
String :: { Annot String }
    : string { (\(Annot r (TokenString x)) -> Annot r x) $1 }
LIdentSimple :: { Annot VarName }
    : lidentSimple { ((\(Annot r (TokenIdentLower (LIdent [] (x:|[])))) -> Annot r x) $1 :: Annot VarName) }
LIdent :: { Annot LIdent }
    : lidentSimple { (\(Annot r (TokenIdentLower x)) -> Annot r x) $1 }
    | lident { (\(Annot r (TokenIdentLower x)) -> Annot r x) $1 }
UIdentSimple :: { Annot TypeName }
    : uidentSimple { (\(Annot r (TokenIdentUpper (UIdent [] x))) -> Annot r x) $1 }
UIdent :: { Annot UIdent }
    : uidentSimple { (\(Annot r (TokenIdentUpper x)) -> Annot r x) $1 }
    | uident { (\(Annot r (TokenIdentUpper x)) -> Annot r x) $1 }
SimpleType :: { Annot Type.SimpleType }
    : simpletype { (\(Annot r (TokenSimpleType x)) -> Annot r x) $1 }

{

tupleOrParens :: [Expr Parsed] -> Expr_ Parsed
tupleOrParens [x] = unAnnot x
tupleOrParens xs =  Tuple xs

typeToModuleIdent :: UIdent -> ResolveableModule
typeToModuleIdent (UIdent modules ident) = modules ++ [ident]

parseError :: Annot Token -> Alex a
parseError (Annot r x) = do
    (_,_,_,rest) <- alexGetInput
    alexError ("parse error " ++ maybe "" ("at " ++) (dispRange r) ++ "\n>"++ take 50 (takeWhile (/='\n') rest))

lexer :: (Annot Token -> Alex a) -> Alex a
lexer = (alexMonadScanAnnot >>=)

}