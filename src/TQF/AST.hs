{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
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
  , LValue(..)
  , BinaryOperator(..)
  , UnaryOperator(..)

  , Parsed
  , ParsedType(..)

  , Resolved
  , ModLIdentDecl(..)
  , IdentKind(..)

  , ToIdent(..)
  , ResolveableModule
  , VarName(..)
  , LIdent(..)
  , TypeName(..)
  , UIdent(..)
  ) where

import           TQF.Type

import           Control.Arrow      (Arrow ((***)))
import           Data.List.Extra    (intercalate, unsnoc)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromJust)
import           Data.String.Pretty
import           TQF.AST.Annotated

data Parsed
data Resolved

type family TypeDeclF a where
  TypeDeclF Parsed = Annot ParsedType
  TypeDeclF Resolved = Annot (Type' String)

type family LIdentF a where
  LIdentF Parsed = LIdent
  LIdentF Resolved = ModLIdentDecl

type family DeclIdentF a where
  DeclIdentF Parsed = VarName
  DeclIdentF Resolved = ModLIdentDecl

type family LValueF a where
  LValueF Parsed = Expr Parsed
  LValueF Resolved = LValue Resolved

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
  , functionTypeParams :: [TypeName]
  , functionArguments  :: [(TypeDeclF a, DeclIdentF a)]
  , functionContent    :: [Statement a]
  } | VariableDecl
  { variableType :: TypeDeclF a
  , variableName :: DeclIdentF a
  } | TypeDecl
  { typeName   :: TypeName
  , typeParams :: [TypeName]
  , typeValue  :: TypeDeclF a
  } | CommandDecl
  { commandSQF        :: String
  , commandName       :: DeclIdentF a
  , commandTypeParams :: [TypeName]
  , commandReturnType :: TypeDeclF a
  , commandArgs       :: [(TypeDeclF a, DeclIdentF a)]
  } | ExternalFunctionDecl
  { functionName       :: DeclIdentF a
  , functionType       :: TypeDeclF a
  , functionTypeParams :: [TypeName]
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
  = VariableDeclaration (TypeDeclF a) VarName (Maybe (Expr a))
  | Assignment (LValueF a) (Expr a)
  | Expr (Expr a)

type Expr a = Annot (Expr_ a)

data Expr_ a
  = Variable (Annot (Ident a))
  | FuncCall (Annot (Ident a)) [Expr a]
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
deriving instance ValidASTLevel a => Show (LValue a)
deriving instance ValidASTLevel a => Eq (LValue a)

data UnaryOperator = NotOp | NegOp
    deriving (Show, Eq)

data BinaryOperator = AndOp | OrOp | AddOp | SubOp | DivOp | MulOp | ModOp | EqOp | NotEqOp | LessOp | GreaterOp | LessEqualOp | GreaterEqualOp
    deriving (Show, Eq)

newtype ParsedType = ParsedType { unParsedType :: Type' (UIdent, [Annot ParsedType])}
    deriving (Show, Ord, Eq)
instance Semigroup ParsedType where
  (<>) :: ParsedType -> ParsedType -> ParsedType
  l <> r = ParsedType $ unParsedType l <> unParsedType r

data UIdent = UIdent ResolveableModule TypeName
  deriving (Show, Eq, Ord)
data LIdent = LIdent ResolveableModule VarName
  deriving (Show, Eq, Ord)

data Ident a
  = Ident
  { identName     :: LIdentF a
  , identTypeArgs :: [TypeDeclF a]
  }

deriving instance ValidASTLevel a => Show (Ident a)
deriving instance ValidASTLevel a => Eq (Ident a)

type ResolveableModule = [TypeName]

newtype TypeName = TypeName {unTypeName:: String}
    deriving (Eq, Ord)

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

data LValue a
  = LValueVar (Annot (Ident a))
  | LValueField (Expr a) (Annot VarName)

instance Show TypeName where
  show (TypeName x) = show x

newtype VarName = VarName {unVarName:: String}
    deriving (Eq, Ord)

instance Show VarName where
  show (VarName x) = show x

class ToIdent a where
  toIdent :: String -> a

instance ToIdent VarName where
  toIdent = VarName
instance ToIdent TypeName where
  toIdent = TypeName
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

instance Pretty VarName where
  prettyPrint = unVarName
instance Pretty TypeName where
  prettyPrint = unTypeName
instance Pretty LIdent where
  prettyPrint (LIdent mod x)
    = concatMap ((++".") . prettyPrint) mod
    ++ prettyPrint x

instance Pretty UIdent where
  prettyPrint (UIdent mod x)
    = concatMap ((++".") . prettyPrint) mod
    ++ prettyPrint x

instance Pretty ResolveableModule where
  prettyPrint = intercalate "." . fmap prettyPrint

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
