{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DataKinds, ConstraintKinds #-}
module TQF.AST where

import TQF.Type

import           Data.List.NonEmpty

data Parsed
data Resolved

type family TypeDeclF a where
  TypeDeclF Parsed = ASTType
  TypeDeclF Resolved = Type 

type family LIdentF a where
  LIdentF Parsed = LIdent
  LIdentF Resolved = ModLIdentDecl

type ValidASTLevel a = (Show (TypeDeclF a), Eq (TypeDeclF a), Show (LIdentF a), Eq (LIdentF a))

data Module a = Module
  { moduleName         :: ResolveableModule
  , moduleImports      :: [ImportStatement]
  , moduleDeclarations :: [Declaration a]
  }

deriving instance ValidASTLevel a => Show (Module a)
deriving instance ValidASTLevel a => Eq (Module a)

data ImportStatement = ImportStatement
  { importName      :: ResolveableModule
  , importQualified :: Bool
  , importAs        :: Maybe ResolveableModule
  }
  deriving (Show, Eq)

data Declaration a = FunctionDecl
  { functionName          :: VarName
  , functionType          :: TypeDeclF a
  , functionArguments     :: [(TypeDeclF a, VarName)]
  , functionContent       :: Statement a
  } | VariableDecl
  { variableType :: TypeDeclF a
  , variableName :: VarName
  } | TypeDecl
  { typeName :: TypeName
  , typeValue :: TypeDeclF a
  } | CommandDecl
  { commandName :: String
  , commandReturnType :: TypeDeclF a
  , commandArgs :: CommandArgs (TypeDeclF a)
  }
    
deriving instance ValidASTLevel a => Show (Declaration a)
deriving instance ValidASTLevel a => Eq (Declaration a)

data CommandArgs a = CommandNular
  | CommandUnary a
  | CommandBinary a a
  deriving (Show, Eq)

instance Functor CommandArgs where
  fmap _ CommandNular = CommandNular
  fmap f (CommandUnary x) = CommandUnary (f x)
  fmap f (CommandBinary x y) = CommandBinary (f x) (f y)
instance Foldable CommandArgs where
  foldr _ acc CommandNular = acc
  foldr f acc (CommandUnary x) = f x acc
  foldr f acc (CommandBinary x y) = f x (f y acc)
instance Traversable CommandArgs where
  traverse f CommandNular = pure CommandNular
  traverse f (CommandUnary x) = CommandUnary <$> f x
  traverse f (CommandBinary x y) = CommandBinary <$> f x <*> f y


data Statement a =
    CodeBlock [Statement a]
    | VariableDeclaration {
        varDeclType:: TypeDeclF a,
        varDeclName:: VarName,
        varDeclValue:: Maybe (Expr a)
    }
    | FunctionCall {
        functionCallName:: LIdentF a,
        functionCallArgs:: [Expr a]
    }
    | Assignment {
        assignmentVariable:: LIdentF a,
        assignmentValue:: Expr a
    }
    | IfStatement {
        ifStatementCondition:: Expr a,
        ifStatementTrue:: Statement a,
        ifStatementFalse:: Maybe (Statement a)
    }
    | WhileLoop {
        whileLoopCondition:: Expr a,
        whileLoopStatement:: Statement a
    }
    | DoWhile {
        doWhileCondition:: Expr a,
        doWhileStatement:: Statement a
    }
    | Return (Maybe (Expr a))

deriving instance ValidASTLevel a => Show (Statement a)
deriving instance ValidASTLevel a => Eq (Statement a)

data Expr a = UnaryOperator UnaryOperator (Expr a)
 | BinaryOperator BinaryOperator (Expr a) (Expr a)
 | Variable (LIdentF a)
 | FuncCall (LIdentF a) [Expr a]
 | BoolLiteral Bool
 | NumLiteral Double
 | StringLiteral String
 | ArrayExpr [Expr a]
 | DirectCall String [Expr a]
 | Cast (TypeDeclF a) (Expr a)

deriving instance ValidASTLevel a => Show (Expr a)
deriving instance ValidASTLevel a => Eq (Expr a)

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

data ModLIdentDecl = ModFunction LIdent [Type] Type
  | ModGlobalVariable LIdent Type
  | ModLocalVariable VarName Type

instance Show TypeName where
  show (TypeName x) = show x

newtype VarName = VarName {unVarName:: String}
    deriving (Eq, Ord)

instance Show VarName where
  show (VarName x) = show x
