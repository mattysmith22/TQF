{-# LANGUAGE RecordWildCards #-}
module TQF.TypeChecker
  ( TypeCheckError(..)
  , TypeCheck
  , runTypeCheck
  , typeCheckExpr
  , typeCheckStatement
  , typeCheckDeclaration
  , numType
  , boolType
  , stringType
  , voidType
  , arrayType
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Void
import           Debug.Trace
import qualified SQF.Commands                  as SQFComm
import           TQF.AST
import           TQF.ModuleResolver
import           TQF.ToSQF.Type
import           Text.ParserCombinators.ReadP   ( look )

data TypeCheckError = TypeMismatch Type Type
    | NotPresent Var
    | IncorrectDeclarationType Var DeclarationType DeclarationType
    | MismatchedArgCount Int Int
    | CannotFindSQFCommand String [Type]
    | NotPrimitiveType Type
    | InvalidDirectCallArgCount Int
    deriving (Show, Eq)

type TypeCheck a = Except TypeCheckError a

runTypeCheck :: TypeCheck a -> Either TypeCheckError a
runTypeCheck = runExcept

numType :: Type
numType = Type [] (TypeName "Num")

boolType :: Type
boolType = Type [] (TypeName "Bool")

stringType :: Type
stringType = Type [] (TypeName "String")

arrayType :: Type
arrayType = Type [] (TypeName "Array")

voidType :: Type
voidType = Type [] (TypeName "Void")

getVariableTypeFromNamespace :: Namespace -> Var -> TypeCheck Type
getVariableTypeFromNamespace namespace var = case findLIdent namespace var of
  Nothing  -> throwError $ NotPresent var
  (Just x) -> case x of
    []                      -> throwError $ NotPresent var
    (FunctionDecl {..} : _) -> throwError $ IncorrectDeclarationType var DeclTypeVar DeclTypeFunc
    (VariableDecl {..} : _) -> return variableType

getFunctionTypeFromNamespace :: Namespace -> Var -> [Type] -> TypeCheck Type
getFunctionTypeFromNamespace namespace var argTypes = case findLIdent namespace var of
  Nothing   -> throwError $ NotPresent var
  (Just xs) -> case filter p xs of
    []                      -> throwError $ NotPresent var
    (FunctionDecl {..} : _) -> return functionType
    (VariableDecl {..} : _) -> error "This should never occur"
 where
  p FunctionDecl {..} = map fst functionArguments == argTypes
  p VariableDecl {..} = False

ensure :: Type -> Type -> TypeCheck ()
ensure checkType actualType | checkType == actualType = return ()
                            | otherwise = throwError $ TypeMismatch checkType actualType

convertToSQFType :: Type -> TypeCheck SQFComm.Type
convertToSQFType typ = case toSQFType typ of
  Nothing -> throwError $ NotPrimitiveType typ
  Just x  -> return x

typeCheckUnOp :: UnaryOperator -> Type -> TypeCheck Type
typeCheckUnOp NotOp t = ensure boolType t >> return boolType
typeCheckUnOp NegOp t = ensure numType t >> return numType

typeCheckBinOp :: BinaryOperator -> Type -> Type -> TypeCheck Type
typeCheckBinOp op = case op of
  AndOp          -> boolOp
  OrOp           -> boolOp
  AddOp          -> numOp
  SubOp          -> numOp
  DivOp          -> numOp
  MulOp          -> numOp
  ModOp          -> numOp
  EqOp           -> twoSameRetBoolOp
  NotEqOp        -> twoSameRetBoolOp
  LessOp         -> twoSameRetBoolOp
  GreaterOp      -> twoSameRetBoolOp
  LessEqualOp    -> twoSameRetBoolOp
  GreaterEqualOp -> twoSameRetBoolOp
 where
  boolOp left right = ensure boolType left >> ensure boolType right >> return boolType
  numOp left right = ensure numType left >> ensure numType right >> return numType
  twoSameRetBoolOp left right = ensure left right >> return boolType

zipArgs :: [a] -> [b] -> TypeCheck [(a, b)]
zipArgs xs ys = if length xs == length ys
  then return $ zip xs ys
  else throwError $ MismatchedArgCount (length xs) (length ys)

typeCheckExpr :: Namespace -> Expr -> TypeCheck Type
typeCheckExpr ns (UnaryOperator op expr) = typeCheckExpr ns expr >>= typeCheckUnOp op
typeCheckExpr ns (BinaryOperator op l r) = do
  lType <- typeCheckExpr ns l
  rType <- typeCheckExpr ns r
  typeCheckBinOp op lType rType
typeCheckExpr ns (Variable var         ) = getVariableTypeFromNamespace ns var
typeCheckExpr ns (FuncCall var argExprs) = do
  argTypes <- mapM (typeCheckExpr ns) argExprs
  getFunctionTypeFromNamespace ns var argTypes
typeCheckExpr lookup (BoolLiteral   _    ) = return boolType
typeCheckExpr lookup (NumLiteral    _    ) = return numType
typeCheckExpr lookup (StringLiteral _    ) = return stringType
typeCheckExpr lookup (Array         exprs) = return arrayType
typeCheckExpr _      (DirectCall name [] ) = case SQFComm.searchForType name () of
  Nothing        -> throwError $ CannotFindSQFCommand name []
  (Just retType) -> return $ fromSQFType retType
typeCheckExpr lookup (DirectCall name [x]) = do
  xType    <- typeCheckExpr lookup x
  xTypeSQF <- convertToSQFType xType
  case SQFComm.searchForType name xTypeSQF of
    Nothing        -> throwError $ CannotFindSQFCommand name [xType]
    (Just retType) -> return $ fromSQFType retType
typeCheckExpr lookup (DirectCall name [x, y]) = do
  xType    <- typeCheckExpr lookup x
  xTypeSQF <- convertToSQFType xType
  yType    <- typeCheckExpr lookup y
  yTypeSQF <- convertToSQFType yType
  case SQFComm.searchForType name (xTypeSQF, yTypeSQF) of
    Nothing        -> throwError $ CannotFindSQFCommand name [xType, yType]
    (Just retType) -> return $ fromSQFType retType
typeCheckExpr _      (DirectCall _   args) = throwError $ InvalidDirectCallArgCount $ length args
typeCheckExpr lookup (Cast       typ expr) = do
  typeCheckExpr lookup expr
  return typ

typeCheckStatement :: Namespace -> Type -> Statement -> TypeCheck Namespace
typeCheckStatement ns retType (CodeBlock stmts) = foldM (`typeCheckStatement` retType) ns stmts
typeCheckStatement ns retType (VariableDeclaration declType declName mDeclValue) = do
  case mDeclValue of
    Nothing  -> return ()
    (Just x) -> do
      exprType <- typeCheckExpr ns x
      ensure declType exprType
  return $ addLocalVar declName declType ns
typeCheckStatement ns retType (Assignment var assignValue) = do
  varTyp  <- getVariableTypeFromNamespace ns var
  exprTyp <- typeCheckExpr ns assignValue
  ensure varTyp exprTyp
  return ns
typeCheckStatement lookup retType (FunctionCall ident args) = do
  typeCheckExpr lookup (FuncCall ident args)
  return lookup
typeCheckStatement lookup retType (IfStatement cond true mFalse) = do
  condType <- typeCheckExpr lookup cond
  ensure boolType condType
  typeCheckStatement lookup retType true
  mapM_ (typeCheckStatement lookup retType) mFalse
  return lookup
typeCheckStatement lookup retType (WhileLoop cond stmt) = do
  condType <- typeCheckExpr lookup cond
  ensure boolType condType
  typeCheckStatement lookup retType stmt
typeCheckStatement lookup retType (DoWhile cond stmt) = do
  condType <- typeCheckExpr lookup cond
  ensure boolType condType
  typeCheckStatement lookup retType stmt
typeCheckStatement lookup retType (Return mExpr) = do
  condType <- fromMaybe voidType <$> mapM (typeCheckExpr lookup) mExpr
  ensure retType condType
  return lookup

typeCheckDeclaration :: Namespace -> Declaration -> TypeCheck ()
typeCheckDeclaration _  VariableDecl{}    = return ()
typeCheckDeclaration ns FunctionDecl {..} = do
  let f ns (typ, ident) = addLocalVar ident typ ns
  let ns' = foldl f ns functionArguments
  typeCheckStatement ns' functionType functionContent
  return ()
