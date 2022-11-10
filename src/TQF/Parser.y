{
module TQF.Parser (parse) where

import TQF.Lexer
import TQF.AST
import qualified TQF.Type as Type
import qualified Data.Map as Map
import Data.List
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Either String }

%token
    module { TokenModule }
    where { TokenWhere }
    import { TokenImport }
    qualified { TokenQualified }
    as { TokenAs }
    if { TokenIf }
    else { TokenElse }
    while { TokenWhile }
    do { TokenDo }
    return { TokenReturn }
    type { TokenType }
    function { TokenFunction }
    global { TokenGlobal }
    top { TokenTop }
    '=' { TokenAssign }
    '(' { TokenOpenP }
    '\'(' { TokenOpenPTuple }
    ')' { TokenCloseP }
    '[' { TokenOpenSquare }
    ']' { TokenCloseSquare }
    '{' { TokenOpenBrace }
    '}' { TokenCloseBrace }
    '<' { TokenOpenArrow }
    '>' { TokenCloseArrow }
    ';' { TokenSemicolon }
    ',' { TokenComma }
    '+' { TokenAdd }
    '-' { TokenSub }
    '*' { TokenMul }
    '/' { TokenDiv }
    '%' { TokenMod }
    '&&' { TokenAnd }
    '||' { TokenOr }
    '==' { TokenEq }
    '<=' { TokenLe }
    '>=' { TokenGe }
    '!=' { TokenNe }
    '!' { TokenNot }
    '|' { TokenPipe }
    ':' { TokenColon }
    bool { TokenBool $$ }
    int { TokenNum $$ }
    string { TokenString $$ }
    lidentSimple { TokenIdentLower (LIdent [] $$)}
    lident { TokenIdentLower $$ }
    uident { TokenIdentUpper $$ }
    simpletype { TokenSimpleType $$ }

%left '||'
%right '&&'

%left '==' '!=' 
%left '<=' '>=' '<' '>'
%left '+' '-'
%left '*' '/' '%'
%left NEG '!'

%%

Module : ModuleDeclaration Imports Declarations {Module $1 $2 (reverse $3)}

ModuleDeclaration : module ModuleIdent where {$2}

Imports : {- empty -} {[]}
    | Import Imports { $1 : $2 }

Import : import ImportQualification ModuleIdent ImportRenaming { ImportStatement $3 $2 $4}

ImportQualification : {- empty -} {False}
    | qualified {True}

ImportRenaming : {- empty -} {Nothing}
    | as ModuleIdent {Just $2}

Declarations : {- empty -} {[]}
    | Declarations Declaration { $2 : $1 }

Type : simpletype { Type.simpleType $1 }
    | int { Type.constNumber $1 }
    | string { Type.constString $1 }
    | bool { Type.constBool $1 }
    | top { Type.top }
    | Type '|' Type { $1 <> $3 }
    | '(' Type ')' { $2 }
    | uident { Type.extra $1 }

    -- Tuple definition
    | '(' ')' { Type.tuple [] }
    | '\'(' TupleElements ')' { Type.tuple $2 }
    | '(' Type ',' Type TupleElements ')' { Type.tuple ( $2 : $4 : $5 ) }

    -- Record definition
    | '{' TypeRecordFields '}' { Type.record $ Map.fromList $2 }
    
TupleElements : {- empty -} {[]}
    | Type { [$1] }
    | Type ',' TupleElements { $1 : $3 }

TypeRecordFields : {- empty -} {[]}
    | TypeRecordField {[$1]}
    | TypeRecordField ',' TypeRecordFields { $1:$3 }

TypeRecordField : lidentSimple ':' Type { (unVarName $1, $3) }

Declaration : function Type lidentSimple '(' FunctionDeclArguments ')' CodeBlock { FunctionDecl $3 $2 $5 (CodeBlock $7) }

FunctionDeclArguments : {- empty -} {[]}
    | FunctionDeclArgument {[$1]}
    | FunctionDeclArgument ',' FunctionDeclArguments {$1:$3}

FunctionDeclArgument : Type lidentSimple {($1, $2)}

CodeBlock : '{' Statements '}' {$2}

Statements : {- empty -} {[]}
    | Statement {[$1]}
    | Statement Statements {$1:$2}

StatementNoEndSemicolon : CodeBlock {CodeBlock $1}
    | if '(' Expr ')' IfStatements {IfStatement $3 (fst $5) (snd $5)}
    | while '(' Expr ')' Statement {WhileLoop $3 $5}

IfStatements : StatementNoSemicolon else Statement {($1, Just $3)}
    | Statement {($1, Nothing)}

Statement : StatementNoEndSemicolon {$1}
    | StatementEndSemicolon ';' {$1}

StatementNoSemicolon : StatementNoEndSemicolon {$1}
    | StatementEndSemicolon {$1}

StatementEndSemicolon : Lident LidentStatement {$2 $1}
    | Type lidentSimple VariableDeclarationAssignment {VariableDeclaration $1 $2 $3}
    | do StatementNoSemicolon while '(' Expr ')' {DoWhile $5 $2}
    | return ReturnValue {Return $2}

ReturnValue : {- empty -} {Nothing}
    | Expr {Just $1}

VariableDeclarationAssignment : {- empty -} {Nothing}
    | '=' Expr {Just $2}

LidentStatement : '=' Expr {\s -> Assignment s $2}
    | '(' ExprList ')' {\s -> FunctionCall s $2}

ExprList : {- empty -} {[]}
    | Expr {[$1]}
    | Expr ',' ExprList {$1:$3}

Expr : Expr '+' Expr {BinaryOperator AddOp $1 $3}
    | Expr '-' Expr {BinaryOperator SubOp $1 $3}
    | Expr '*' Expr {BinaryOperator MulOp $1 $3}
    | Expr '/' Expr {BinaryOperator DivOp $1 $3}
    | Expr '%' Expr {BinaryOperator ModOp $1 $3}
    | Expr '&&' Expr {BinaryOperator AndOp $1 $3}
    | Expr '||' Expr {BinaryOperator OrOp $1 $3}
    | Expr '==' Expr {BinaryOperator EqOp $1 $3}
    | Expr '!=' Expr {BinaryOperator NotEqOp $1 $3}
    | Expr '<=' Expr {BinaryOperator LessEqualOp $1 $3}
    | Expr '>=' Expr {BinaryOperator GreaterEqualOp $1 $3}
    | Expr '<' Expr {BinaryOperator LessOp $1 $3}
    | Expr '>' Expr {BinaryOperator GreaterOp $1 $3}
    | '!' Expr {UnaryOperator NotOp $2}
    | '-' Expr %prec NEG {UnaryOperator NegOp $2}

    | Lident {Variable $1}
    | Lident '(' ExprList ')' {FuncCall $1 $3}
    | bool {BoolLiteral $1}
    | int {NumLiteral $1}
    | '-' int {NumLiteral (negate $2)}
    | string {StringLiteral $1}
    | '[' ExprList ']' {ArrayExpr $2}
    | '<' lidentSimple '>' '(' ExprList ')' {DirectCall (unVarName $2) $5}
    | '(' Expr ')' {$2}
    | '(' Type ')' Expr {Cast $2 $4}

ModuleIdent : uident {typeToModuleIdent $1}

Lident : lidentSimple { LIdent [] $1 }
    | lident { $1 }

{
typeToModuleIdent :: UIdent -> ResolveableModule
typeToModuleIdent (UIdent modules ident) = modules ++ [ident]

parseError tokens = Left $ "Parse error on tokens " ++ intercalate " " (show <$> tokens)
}