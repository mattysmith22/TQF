module TQF.AST where

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
  { functionQualifiers    :: [FunctionQualifier]
  , functionName          :: VarName
  , functionType          :: Type
  , functionCaptureGroups :: [(Type, VarName)]
  , functionArguments     :: [(Type, VarName)]
  , functionContent       :: Statement
  } | VariableDecl
  { variableType :: Type,
    variableName :: VarName
  }
  deriving (Show, Eq)

data FunctionQualifier = QualifierExtern
  deriving (Show, Eq)

data Statement =
    CodeBlock [Statement]
    | VariableDeclaration {
        varDeclType:: Type,
        varDeclName:: VarName,
        varDeclValue:: Maybe Expr
    }
    | FunctionCall {
        functionCallName:: Var,
        functionCallArgs:: [Expr]
    }
    | Assignment {
        assignmentVariable:: Var,
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
 | Variable Var
 | FuncCall Var [Expr]
 | BoolLiteral Bool
 | NumLiteral String
 | StringLiteral String
 | Array [Expr]
 | DirectCall String [Expr]
 | Cast Type Expr
 deriving (Show, Eq)

data UnaryOperator = NotOp | NegOp
    deriving (Show, Eq)

data BinaryOperator = AndOp | OrOp | AddOp | SubOp | DivOp | MulOp | ModOp | EqOp | NotEqOp | LessOp | GreaterOp | LessEqualOp | GreaterEqualOp
    deriving (Show, Eq)

data Type = Type ResolveableModule TypeName
  deriving (Show, Eq)
data Var = Var ResolveableModule VarName
  deriving (Show, Eq)

type ResolveableModule = [TypeName]

newtype TypeName = TypeName {unTypeName:: String}
    deriving (Eq, Ord)

instance Show TypeName where
  show (TypeName x) = show x

newtype VarName = VarName {unVarName:: String}
    deriving (Eq, Ord)

instance Show VarName where
  show (VarName x) = show x
