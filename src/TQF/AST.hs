module TQF.AST where

import TQF.Type

import           Data.List.NonEmpty

data Module = Module
  { moduleName         :: ResolveableModule
  , moduleImports      :: [ImportStatement]
  , moduleDeclarations :: [Declaration]
  }
  deriving (Show, Eq)

data ImportStatement = ImportStatement
  { importName      :: ResolveableModule
  , importQualified :: Bool
  , importAs        :: Maybe ResolveableModule
  }
  deriving (Show, Eq)

data DeclarationType = DeclTypeFunc | DeclTypeVar
  deriving (Show, Eq)

declToType :: Declaration -> DeclarationType
declToType FunctionDecl{} = DeclTypeFunc
declToType VariableDecl{} = DeclTypeVar

data Declaration = FunctionDecl
  { functionName          :: VarName
  , functionType          :: ASTType
  , functionArguments     :: [(ASTType, VarName)]
  , functionContent       :: Statement
  } | VariableDecl
  { variableType :: ASTType,
    variableName :: VarName
  }
  deriving (Show, Eq)

data Statement =
    CodeBlock [Statement]
    | VariableDeclaration {
        varDeclType:: ASTType,
        varDeclName:: VarName,
        varDeclValue:: Maybe Expr
    }
    | FunctionCall {
        functionCallName:: LIdent,
        functionCallArgs:: [Expr]
    }
    | Assignment {
        assignmentVariable:: LIdent,
        assignmentValue:: Expr
    }
    | IfStatement {
        ifStatementCondition:: Expr,
        ifStatementTrue:: Statement,
        ifStatementFalse:: Maybe Statement
    }
    | WhileLoop {
        whileLoopCondition:: Expr,
        whileLoopStatement:: Statement
    }
    | DoWhile {
        doWhileCondition:: Expr,
        doWhileStatement:: Statement
    }
    | Return (Maybe Expr)
    deriving (Show, Eq)

data Expr = UnaryOperator UnaryOperator Expr
 | BinaryOperator BinaryOperator Expr Expr
 | Variable LIdent
 | FuncCall LIdent [Expr]
 | BoolLiteral Bool
 | NumLiteral Double
 | StringLiteral String
 | ArrayExpr [Expr]
 | DirectCall String [Expr]
 | Cast ASTType Expr
 deriving (Show, Eq)

data UnaryOperator = NotOp | NegOp
    deriving (Show, Eq)

data BinaryOperator = AndOp | OrOp | AddOp | SubOp | DivOp | MulOp | ModOp | EqOp | NotEqOp | LessOp | GreaterOp | LessEqualOp | GreaterEqualOp
    deriving (Show, Eq)

type ASTType = Type' UIdent

data UIdent = UIdent ResolveableModule TypeName
  deriving (Show, Eq, Ord)
data LIdent = LIdent ResolveableModule VarName
  deriving (Show, Eq, Ord)

type ResolveableModule = [TypeName]

newtype TypeName = TypeName {unTypeName:: String}
    deriving (Eq, Ord)

instance Show TypeName where
  show (TypeName x) = show x

newtype VarName = VarName {unVarName:: String}
    deriving (Eq, Ord)

instance Show VarName where
  show (VarName x) = show x
