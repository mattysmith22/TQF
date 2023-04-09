{
{-# LANGUAGE TypeFamilies #-}
module TQF.Parser (parse) where

import TQF.Lexer
import TQF.AST
import TQF.AST.Annotated
import qualified TQF.Type as Type
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe
import Data.List
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
    then      { Annot _ TokenThen }
    exitWith  { Annot _ TokenExitWith }
    else      { Annot _ TokenElse }
    while     { Annot _ TokenWhile }
    var       { Annot _ TokenVar }
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
    lidentSimple { $$@(Annot _ (TokenIdentLower (LIdent [] _)))}
    lident { $$@(Annot _ (TokenIdentLower _)) }
    uidentSimple { $$@(Annot _ (TokenIdentUpper (UIdent [] _)))}
    uident { $$@(Annot _ (TokenIdentUpper _)) }
    nil { $$@(Annot _ (TokenSimpleType Type.Nil))}
    simpletype { $$@(Annot _ (TokenSimpleType _)) }
    fieldaccess { $$@(Annot _ (TokenFieldAccess _))}

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

Type :: { Annot ParsedType }
    : BaseType { $1 }
    | BaseType '|' Type { $1 <> $3 }

BaseType :: { Annot ParsedType }
    : SimpleType { fmap (ParsedType . Type.simpleType) $1 }
    | Num { fmap (ParsedType . Type.constNumber) $1 }
    | String { fmap (ParsedType . Type.constString) $1 }
    | Bool { fmap (ParsedType . Type.constBool) $1 }
    | top { Annot (pos $1) $ ParsedType Type.top }
    | '(' Type ')' { Annot (pos $1 <> pos $3) (unAnnot $2) }
    | UIdent TypeArgs { Annot (pos $1) $ ParsedType $ Type.extra (unAnnot $1, $2) }

    -- Tuple definition
    | '[' Type ']' {Annot (pos $1 <> pos $3) $ ParsedType $ Type.array $ unParsedType $ unAnnot $2}
    | '(' ')' { Annot (pos $1 <> pos $2) $ ParsedType $ Type.tuple [] }
    | '\'(' TupleElements ')' { Annot (pos $1 <> pos $3) $ ParsedType $ Type.tuple (fmap (unParsedType . unAnnot) $2) }
    | '(' Type ',' Type TupleElements ')' { Annot (pos $1 <> pos $6) $ ParsedType $ Type.tuple (unParsedType (unAnnot $2) :unParsedType (unAnnot $4) : (fmap (unParsedType . unAnnot) $5) ) }

    -- Record definition
    | '{' TypeRecordFields '}' { Annot (pos $1 <> pos $3) $ ParsedType $ Type.record $ Map.fromList $ fmap (second unParsedType) $2 }

TypeArgs :: { [Annot ParsedType] }
    : {- empty -} {[]}
    | '<' TupleElements '>' {$2}

TupleElements :: { [Annot ParsedType] }
    : {- empty -} {[]}
    | Type { [$1] }
    | Type ',' TupleElements {$1 : $3 }

TypeRecordFields :: { [(String, ParsedType)] }
    : {- empty -} {[]}
    | TypeRecordField {[$1]}
    | TypeRecordField ',' TypeRecordFields { $1:$3 }

TypeRecordField :: { (String, ParsedType) }
    : LIdentSimple ':' Type { (unVarName $ unAnnot $1, unAnnot $3) }

Declaration :: { Declaration Parsed }
    : function LIdentSimple TypeParamsDecl '(' FunctionDeclArguments ')' ':' Type CodeBlock { Annot (pos $1 <> pos $9) $ FunctionDecl (unAnnot $2) $8 $3 $5 (unAnnot $9) }
    | type UIdentSimple TypeParamsDecl '=' Type { Annot (pos $1 <> pos $5) $ TypeDecl (unAnnot $2) $3 $5 }
    | global LIdentSimple ':' Type { Annot (pos $1 <> pos $4) $ VariableDecl $4 (unAnnot $2) }
    | command LIdentSimple TypeParamsDecl '(' FunctionDeclArguments ')' ':' Type '=' String { Annot (pos $1 <> pos $10) $ CommandDecl (unAnnot $10) (unAnnot $2) $3 $8 $5 }
    | external function LIdentSimple TypeParamsDecl '(' FunctionDeclArguments ')' ':' Type '=' String { Annot (pos $1 <> pos $10) $ ExternalFunctionDecl (unAnnot $3) $9 $4 $6 (unAnnot $11) }
    | external LIdentSimple ':' Type '=' String { Annot (pos $1 <> pos $6) $ ExternalVariableDecl (unAnnot $2) $4 (unAnnot $6) }

TypeParamsDecl :: { [TypeName] }
TypeParamsDecl
    : {- empty -} {[]}
    | '<' TypeParamArgsDecl '>' {$2}

TypeParamArgsDecl :: { [TypeName] }
TypeParamArgsDecl
    : {- empty -} {[]}
    | UIdentSimple {[unAnnot $1]}
    | UIdentSimple ',' TypeParamArgsDecl {unAnnot $1 : $3}

TypeParams :: { [Annot ParsedType] }
TypeParams
    : {- empty -} {[]}
    | '<' TypeParamArgs '>' {$2}

TypeParamArgs :: { [Annot ParsedType] }
TypeParamArgs
    : {- empty -} {[]}
    | Type {[$1]}
    | Type ',' TypeParamArgs {$1 : $3}


FunctionDeclArguments :: { [(Annot ParsedType, VarName)] }
    : {- empty -} {[]}
    | FunctionDeclArgument {[$1]}
    | FunctionDeclArgument ',' FunctionDeclArguments {$1:$3}

FunctionDeclArgument :: { (Annot ParsedType, VarName) }
    : LIdentSimple ':' Type {($3, unAnnot $1)}

CodeBlock :: { Annot [Statement Parsed] }
    : '{' Statements '}' {Annot (pos $1 <> pos $3) $2}

Statements :: { [Statement Parsed] }
    : {- empty -} {[]}
    | Statement ';' Statements {$1:$3}

IfStatements :: { Annot (IfTrue [Statement Parsed]) }
    : then CodeBlock MElse {Annot (pos $1 <> pos $2 <> pos $3) $ ThenDo (unAnnot $2) (unAnnot $3)}
    | exitWith CodeBlock {Annot (pos $1 <> pos $2) $ ThenExitWith (unAnnot $2)}

MElse :: { Annot (Maybe [Statement Parsed]) }
    : {- empty -} { Annot NoPlace Nothing}
    | else CodeBlock { Annot (pos $1 <> pos $2) $ Just (unAnnot $2) }

Statement :: { Statement Parsed }
    : var LIdentSimple ':' Type VariableDeclarationAssignment {Annot ((pos $1 <> pos $4) <> fromMaybe NoPlace (fmap pos $5)) $ VariableDeclaration $4 (unAnnot $2) $5}
    | ParsedValue '=' Expr {Annot (pos $1 <> pos $3) $ Assignment $1 $3}
    | Expr { Annot (pos $1) $ Expr $1 }

VariableDeclarationAssignment :: { Maybe (Expr Parsed) }
    : {- empty -} {Nothing}
    | '=' Expr {Just $2}

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
    | ParsedValue {Annot (pos $1) $ Variable $1}
    | ParsedValue '(' ExprList ')' {Annot (pos $1 <> pos $4) $ FuncCall $1 $3}
    | Bool {fmap BoolLiteral $1}
    | Num {fmap NumLiteral $1}
    | String {fmap StringLiteral $1}
    | '[' ExprList ']' {Annot (pos $1 <> pos $3) $ ArrayExpr $2}
    | '(' ExprList ')' { Annot (pos $1 <> pos $3) $ tupleOrParens $2}
    | '\'(' ExprList ')' { Annot (pos $1 <> pos $3) $ Tuple $2}
    | '<' Type '>' Expr {Annot (pos $1 <> pos $4) $ Cast $2 $4}
    | if '(' Expr ')' IfStatements {Annot (pos $1 <> pos $5) $ IfStatement $3 (unAnnot $5)}
    | while '(' Expr ')' CodeBlock {Annot (pos $1 <> pos $5) $ WhileLoop $3 (unAnnot $5)}
    | nil {Annot (pos $1) NilLit}

FieldAccesses :: {[Annot VarName]}
    : {- empty -} {[]}
    | FieldAccess FieldAccesses {$1:$2}

ParsedValue :: {Annot ParsedValue}
    : LIdent TypeParams FieldAccesses {Annot (pos $1 <> mconcat (fmap pos $2) <> mconcat (fmap pos $3)) $ ParsedValue (unAnnot $1) $2 $3}

ModuleIdent :: { Annot ResolveableModule }
    : UIdent {fmap typeToModuleIdent $1}

Bool :: { Annot Bool }
    : bool { (\(Annot r (TokenBool x)) -> Annot r x) $1 }
Num :: { Annot Double }
    : int { (\(Annot r (TokenNum x)) -> Annot r x) $1 }
String :: { Annot String }
    : string { (\(Annot r (TokenString x)) -> Annot r x) $1 }
LIdentSimple :: { Annot VarName }
    : lidentSimple { ((\(Annot r (TokenIdentLower (LIdent [] x))) -> Annot r x) $1 :: Annot VarName) }
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
    | nil { (\(Annot r (TokenSimpleType x)) -> Annot r x) $1 }
FieldAccess :: { Annot VarName }
    : fieldaccess { (\(Annot r (TokenFieldAccess x)) -> Annot r x) $1}

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