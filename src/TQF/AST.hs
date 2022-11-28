{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DataKinds, ConstraintKinds, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module TQF.AST where

import TQF.Type

import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Char (isLower)
import qualified Data.List.NonEmpty as NE
import           Data.List.Extra (unsnoc, intercalate)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)
import           Data.String.Pretty
import           Control.Arrow
import           SQF.Commands
import           TQF.AST.Annotated

data Parsed
data Resolved

type family TypeDeclF a where
  TypeDeclF Parsed = Annot ASTType
  TypeDeclF Resolved = Annot Type 

type family LIdentF a where
  LIdentF Parsed = Annot LIdent
  LIdentF Resolved = Annot ResolvedLIdent

type family DeclIdentF a where
  DeclIdentF Parsed = VarName
  DeclIdentF Resolved = ModLIdentDecl

type family CommandF a where
  CommandF Parsed = String
  CommandF Resolved = (String, [(CommandArgs Type, Type)])

type ValidASTLevel a = (Show (DeclIdentF a), Eq (DeclIdentF a), Show (TypeDeclF a), Eq (TypeDeclF a), Show (LIdentF a), Eq (LIdentF a), Show (CommandF a), Eq (CommandF a))

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
  { functionName          :: DeclIdentF a
  , functionType          :: TypeDeclF a
  , functionArguments     :: [(TypeDeclF a, DeclIdentF a)]
  , functionContent       :: [Expr a]
  } | VariableDecl
  { variableType :: TypeDeclF a
  , variableName :: DeclIdentF a
  } | TypeDecl
  { typeName :: TypeName
  , typeValue :: TypeDeclF a
  } | CommandDecl
  { commandSQF :: String
  , commandName :: DeclIdentF a
  , commandReturnType :: TypeDeclF a
  , commandArgs :: [(TypeDeclF a, DeclIdentF a)]
  } | ExternalFunctionDecl
  { functionName          :: DeclIdentF a
  , functionType          :: TypeDeclF a
  , functionArguments     :: [(TypeDeclF a, DeclIdentF a)]
  , functionSQFName       :: String
  } | ExternalVariableDecl
  { variableName          :: DeclIdentF a
  , variableType          :: TypeDeclF a
  , variableSQFName       :: String
  }
    
deriving instance ValidASTLevel a => Show (Declaration_ a)
deriving instance ValidASTLevel a => Eq (Declaration_ a)

instance Functor CommandArgs where
  fmap _ CommandNular = CommandNular
  fmap f (CommandUnary x) = CommandUnary (f x)
  fmap f (CommandBinary x y) = CommandBinary (f x) (f y)
instance Foldable CommandArgs where
  foldr _ acc CommandNular = acc
  foldr f acc (CommandUnary x) = f x acc
  foldr f acc (CommandBinary x y) = f x (f y acc)
instance Traversable CommandArgs where
  traverse :: Applicative f => (a -> f b) -> CommandArgs a -> f (CommandArgs b)
  traverse f CommandNular = pure CommandNular
  traverse f (CommandUnary x) = CommandUnary <$> f x
  traverse f (CommandBinary x y) = CommandBinary <$> f x <*> f y

type Expr a = Annot (Expr_ a) 

data Expr_ a
  = Variable (LIdentF a)
  | FuncCall (LIdentF a) [Expr a]
  | BoolLiteral Bool
  | UnOp (Annot UnaryOperator) (Expr a)
  | BinOp (Annot BinaryOperator) (Expr a) (Expr a)
  | NumLiteral Double
  | StringLiteral String
  | ArrayExpr [Expr a]
  | Cast (TypeDeclF a) (Expr a)
  | Tuple [Expr a]
  | VariableDeclaration (TypeDeclF a) VarName (Maybe (Expr a))
  | Assignment (LIdentF a) (Expr a)
  | IfStatement (Expr a) (IfTrue [Expr a])
  | WhileLoop (Expr a) [Expr a]

data IfTrue a
  = ThenDo a (Maybe a)
  | ThenExitWith a
  deriving (Show, Eq)

instance Functor IfTrue where
  fmap f (ThenDo t mf) = ThenDo (f t) (fmap f mf)
  fmap f (ThenExitWith x) = ThenExitWith $ f x

instance Foldable IfTrue where
  foldMap f (ThenDo t mf) = f t <> foldMap f mf
  foldMap f (ThenExitWith x) = f x

instance Traversable IfTrue where
  traverse f (ThenDo t mf) = ThenDo <$> f t <*> traverse f mf
  traverse f (ThenExitWith x) = ThenExitWith <$> f x

deriving instance ValidASTLevel a => Show (Expr_ a)
deriving instance ValidASTLevel a => Eq (Expr_ a)

data UnaryOperator = NotOp | NegOp
    deriving (Show, Eq)

data BinaryOperator = AndOp | OrOp | AddOp | SubOp | DivOp | MulOp | ModOp | EqOp | NotEqOp | LessOp | GreaterOp | LessEqualOp | GreaterEqualOp
    deriving (Show, Eq)

type ASTType = Type' UIdent

data UIdent = UIdent ResolveableModule TypeName
  deriving (Show, Eq, Ord)
data LIdent = LIdent ResolveableModule (NonEmpty VarName)
  deriving (Show, Eq, Ord)

data ResolvedLIdent = ResolvedLIdent ModLIdentDecl [VarName]
  deriving (Show, Eq)

type ResolveableModule = [TypeName]

newtype TypeName = TypeName {unTypeName:: String}
    deriving (Eq, Ord)

data IdentKind = ValueKind | NularCommandKind | UnaryCommandKind | BinaryCommandKind
    deriving (Show, Eq)
data ModLIdentDecl = ModLIdentDecl
  { lIdentModule :: ResolveableModule
  , lIdentName :: VarName
  , lIdentType :: Type
  , lIdentKind :: IdentKind
  , lIdentSQFName :: String
  }
  deriving (Show, Eq)

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
    . second NE.fromList
    . (fmap TypeName *** fmap VarName)
    . break (isLower . head)
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
    ++ intercalate "." (prettyPrint <$> NE.toList x)

instance Pretty UIdent where
  prettyPrint (UIdent mod x)
    = concatMap ((++".") . prettyPrint) mod
    ++ prettyPrint x

instance Pretty ResolveableModule where
  prettyPrint = intercalate "." . fmap prettyPrint

instance Pretty BinaryOperator where
  prettyPrint AndOp = "&&"
  prettyPrint OrOp = "||"
  prettyPrint AddOp = "+"
  prettyPrint SubOp = "-"
  prettyPrint DivOp = "/"
  prettyPrint MulOp = "*"
  prettyPrint ModOp = "%"
  prettyPrint EqOp = "=="
  prettyPrint NotEqOp = "!="
  prettyPrint LessOp = "<"
  prettyPrint GreaterOp = ">"
  prettyPrint LessEqualOp = "<="
  prettyPrint GreaterEqualOp = ">="

instance Pretty UnaryOperator where
  prettyPrint NotOp = "!"
  prettyPrint NegOp = "-"