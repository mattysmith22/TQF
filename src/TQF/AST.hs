{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TQF.AST
  ( Module(..)
  , ImportStatement(..)
  , Declaration
  , Declaration_(..)
  , Statement
  , Statement_(..)
  , Expr
  , Expr_(..)
  , IfTrue(..)
  , Ident(..)
  , BinaryOperator(..)
  , UnaryOperator(..)

  , ModLIdentDecl(..)
  , IdentKind(..)

  , ToIdent(..)
  , ResolveableModule
  , VarName(..)
  , LIdent(..)
  , TypeName(..)
  , UIdent(..)

  , TypeDeclF
  , LIdentF
  , DeclIdentF
  , LValueF
  ) where

import           TQF.Type

import           Control.Arrow      (Arrow ((***)))
import           Data.List.Extra    (unsnoc)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromJust)
import           Data.String.Pretty
import           TQF.AST.Annotated
import           TQF.Types

type family TypeDeclF a :: *
type family LIdentF a :: *
type family DeclIdentF a :: *
type family LValueF a :: *

type ValidASTLevel a = (Show (DeclIdentF a), Eq (DeclIdentF a), Show (TypeDeclF a), Eq (TypeDeclF a), Show (LIdentF a), Eq (LIdentF a), Show(LValueF a), Eq (LValueF a))

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

type Declaration a = Annot (Declaration_ a)

data Declaration_ a = FunctionDecl
  { functionName       :: DeclIdentF a
  , functionType       :: TypeDeclF a
  , functionTypeParams :: [Annot TypeName]
  , functionArguments  :: [(TypeDeclF a, DeclIdentF a)]
  , functionContent    :: [Statement a]
  } | VariableDecl
  { variableType :: TypeDeclF a
  , variableName :: DeclIdentF a
  } | TypeDecl
  { typeName   :: Annot TypeName
  , typeParams :: [Annot TypeName]
  , typeValue  :: TypeDeclF a
  } | CommandDecl
  { commandSQF        :: String
  , commandName       :: DeclIdentF a
  , commandTypeParams :: [Annot TypeName]
  , commandReturnType :: TypeDeclF a
  , commandArgs       :: [(TypeDeclF a, DeclIdentF a)]
  } | ExternalFunctionDecl
  { functionName       :: DeclIdentF a
  , functionType       :: TypeDeclF a
  , functionTypeParams :: [Annot TypeName]
  , functionArguments  :: [(TypeDeclF a, DeclIdentF a)]
  , functionSQFName    :: String
  } | ExternalVariableDecl
  { variableName    :: DeclIdentF a
  , variableType    :: TypeDeclF a
  , variableSQFName :: String
  }

deriving instance ValidASTLevel a => Show (Declaration_ a)
deriving instance ValidASTLevel a => Eq (Declaration_ a)

type Statement a = Annot (Statement_ a)

data Statement_ a
  = VariableDeclaration (TypeDeclF a) (DeclIdentF a) (Maybe (Expr a))
  | Assignment (LValueF a) (Expr a)
  | Expr (Expr a)

type Expr a = Annot (Expr_ a)

data Expr_ a
  = Variable (Annot (Ident a))
  | FuncCall (Expr a) [Expr a]
  | BoolLiteral Bool
  | UnOp (Annot UnaryOperator) (Expr a)
  | BinOp (Annot BinaryOperator) (Expr a) (Expr a)
  | NumLiteral Double
  | StringLiteral String
  | ArrayExpr [Expr a]
  | Cast (TypeDeclF a) (Expr a)
  | Tuple [Expr a]
  | FieldAccess (Expr a) (Annot VarName)
  | IfStatement (Expr a) (IfTrue [Statement a])
  | WhileLoop (Expr a) [Statement a]
  | NilLit

data IfTrue a
  = ThenDo a (Maybe a)
  | ThenExitWith a
  deriving (Show, Eq)

instance Functor IfTrue where
  fmap f (ThenDo t mf)    = ThenDo (f t) (fmap f mf)
  fmap f (ThenExitWith x) = ThenExitWith $ f x

instance Foldable IfTrue where
  foldMap f (ThenDo t mf)    = f t <> foldMap f mf
  foldMap f (ThenExitWith x) = f x

instance Traversable IfTrue where
  traverse f (ThenDo t mf)    = ThenDo <$> f t <*> traverse f mf
  traverse f (ThenExitWith x) = ThenExitWith <$> f x

deriving instance ValidASTLevel a => Show (Expr_ a)
deriving instance ValidASTLevel a => Eq (Expr_ a)
deriving instance ValidASTLevel a => Show (Statement_ a)
deriving instance ValidASTLevel a => Eq (Statement_ a)

data UnaryOperator = NotOp | NegOp
    deriving (Show, Eq)

data BinaryOperator = AndOp | OrOp | AddOp | SubOp | DivOp | MulOp | ModOp | EqOp | NotEqOp | LessOp | GreaterOp | LessEqualOp | GreaterEqualOp
    deriving (Show, Eq)

data UIdent = UIdent ResolveableModule TypeName
  deriving (Show, Eq, Ord)
data LIdent = LIdent ResolveableModule VarName
  deriving (Show, Eq, Ord)

instance Pretty LIdent where
  prettyPrint (LIdent mod x)
    = concatMap ((++".") . prettyPrint) mod
    ++ prettyPrint x

instance Pretty UIdent where
  prettyPrint (UIdent mod x)
    = concatMap ((++".") . prettyPrint) mod
    ++ prettyPrint x

instance ToIdent LIdent where
  toIdent
    = uncurry LIdent
    . (fmap TypeName *** VarName)
    . fromJust
    . unsnoc
    . splitOn "."
instance ToIdent UIdent where
  toIdent
    = uncurry UIdent
    . fromJust
    . unsnoc
    . fmap TypeName
    . splitOn "."

data Ident a
  = Ident
  { identName     :: LIdentF a
  , identTypeArgs :: [TypeDeclF a]
  }

deriving instance ValidASTLevel a => Show (Ident a)
deriving instance ValidASTLevel a => Eq (Ident a)

type ResolveableModule = [TypeName]

data IdentKind = ValueKind | NularCommandKind | UnaryCommandKind | BinaryCommandKind
    deriving (Show, Eq)
data ModLIdentDecl
  = ModLIdentDecl
  { lIdentModule  :: ResolveableModule
  , lIdentName    :: VarName
  , lIdentType    :: GenericType
  , lIdentKind    :: IdentKind
  , lIdentSQFName :: String
  }
  | ModLIdentLocal
  { lIdentName    :: VarName
  , lIdentType    :: GenericType
  , lIdentKind    :: IdentKind
  , lIdentSQFName :: String
  , lIdentId      :: Int
    -- ^There may be multiple nested scopes with idents of the same name.
    -- @lIdentId@ is used to differentiate these as well as lIdentName
  }
  deriving (Show, Eq)

instance Pretty BinaryOperator where
  prettyPrint AndOp          = "&&"
  prettyPrint OrOp           = "||"
  prettyPrint AddOp          = "+"
  prettyPrint SubOp          = "-"
  prettyPrint DivOp          = "/"
  prettyPrint MulOp          = "*"
  prettyPrint ModOp          = "%"
  prettyPrint EqOp           = "=="
  prettyPrint NotEqOp        = "!="
  prettyPrint LessOp         = "<"
  prettyPrint GreaterOp      = ">"
  prettyPrint LessEqualOp    = "<="
  prettyPrint GreaterEqualOp = ">="

instance Pretty UnaryOperator where
  prettyPrint NotOp = "!"
  prettyPrint NegOp = "-"
