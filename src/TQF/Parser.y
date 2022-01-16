{
module TQF.Parser (parse) where

import TQF.Lexer
import TQF.AST
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
    extern { TokenExtern }
    if { TokenIf }
    else { TokenElse }
    while { TokenWhile }
    do { TokenDo }
    return { TokenReturn }
    '=' { TokenAssign }
    '(' { TokenOpenP }
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
    bool { TokenBool $$ }
    int { TokenNum $$ }
    string { TokenString $$ }
    lidentSimple { TokenIdentLower (Var [] $$)}
    lident { TokenIdentLower $$ }
    uident { TokenIdentUpper $$ }

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

Declaration : FunctionQualifiers uident lidentSimple FunctionDeclCapture '(' FunctionDeclArguments ')' CodeBlock { FunctionDecl $1 $3 $2 $4 $6 (CodeBlock $8) }

FunctionDeclCapture : {- empty -} { [] }
    | '[' FunctionDeclArguments ']' { $2 }

FunctionQualifiers : {- empty -} {[]}
    | FunctionQualifier {[$1]}
    | FunctionQualifier FunctionQualifiers {$1:$2}

FunctionQualifier : extern { QualifierExtern }

FunctionDeclArguments : {- empty -} {[]}
    | FunctionDeclArgument {[$1]}
    | FunctionDeclArgument ',' FunctionDeclArguments {$1:$3}

FunctionDeclArgument : uident lidentSimple {($1, $2)}

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
    | uident lidentSimple VariableDeclarationAssignment {VariableDeclaration $1 $2 $3}
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
    | string {StringLiteral $1}
    | '[' ExprList ']' {Array $2}
    | '<' lidentSimple '>' '(' ExprList ')' {DirectCall (unVarName $2) $5}
    | '(' Expr ')' {$2}
    | '(' uident ')' Expr {Cast $2 $4}

ModuleIdent : uident {typeToModuleIdent $1}

Lident : lidentSimple { Var [] $1 }
    | lident { $1 }

{
typeToModuleIdent :: Type -> ResolveableModule
typeToModuleIdent (Type modules ident) = modules ++ [ident]

parseError tokens = Left $ "Parse error on tokens " ++ intercalate " " (show <$> tokens)
}