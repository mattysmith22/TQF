{-# LANGUAGE RecordWildCards #-}
module TQF.TypeCheck
    ( TypeCheckErr(..)
    , typeCheck
    ) where

import           Control.Arrow
import           Control.Monad.Trans.Class
import           Data.Either.Extra
import           Data.Maybe
import           Data.Monoid
import           Data.String.Pretty
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve               (resolveGenericType)
import           TQF.Type                  (SimpleType (Bool, Nil, Number, String), Type, Type', array, constBool,
                                            constNumber, constString, isWithin, lookupField, simpleType, tuple,
                                            validateBinOp, validateFuncCall, validateUnOp)
import           TQF.TypeCheck.Monad

data TypeCheckErr = NotWithin (Annot (Type' String)) (Annot (Type' String))
     | NoField (Annot VarName) (Annot (Type' String))
     | InvalidBinOp (Annot BinaryOperator) (Type' String) (Type' String)
     | InvalidUnOp (Annot UnaryOperator) (Type' String)
     | ExpectedCode Range
     | NotFound (Annot UIdent)
     deriving (Show, Eq)

type T a = TypeCheck () (Type' String) (Either TypeCheckErr) a

instance Pretty TypeCheckErr where
    prettyPrint (NotWithin l r) = "type\n" ++ prettyPrint l ++ "\nis not within type\n" ++ prettyPrint r
    prettyPrint (NoField name typ) = "type\n" ++ prettyPrint typ ++ "\n does not have a field " ++ prettyPrint name
    prettyPrint (InvalidBinOp op l r) = "Invalid arguments to binary operator " ++ prettyPrint op ++ ": " ++ prettyPrint l ++ " and " ++ prettyPrint r
    prettyPrint (InvalidUnOp op x) = "Invalid argument to unary operator " ++ prettyPrint op ++ ": " ++ prettyPrint x
    prettyPrint (ExpectedCode r) = "Expected code " ++ prettyPrint r
    prettyPrint (NotFound x) = "Not found: " ++ prettyPrint x

shouldBeWithin :: Annot (Type' String) -> Annot (Type' String) -> T ()
shouldBeWithin s@(Annot _ small) l@(Annot _ large) = if small `isWithin` large then return () else lift $ Left $ NotWithin s l

typeCheck :: Module Resolved -> Either TypeCheckErr ()
typeCheck Module{..} = runTypeCheck $ mapM_ typeCheckDeclaration moduleDeclarations

typeCheckDeclaration :: Declaration Resolved -> T ()
typeCheckDeclaration (Annot _ FunctionDecl{..}) = do
    blockType <- typeCheckBlock functionContent
    blockType `shouldBeWithin` functionType
typeCheckDeclaration (Annot _ VariableDecl{}) = return ()
typeCheckDeclaration (Annot _ TypeDecl{}) = return ()
typeCheckDeclaration (Annot _ CommandDecl{}) = return ()
typeCheckDeclaration (Annot _ ExternalFunctionDecl{}) = return ()
typeCheckDeclaration (Annot _ ExternalVariableDecl{}) = return ()

typeCheckStmt :: Statement Resolved -> T (Annot Type)
typeCheckStmt (Annot r (VariableDeclaration typ _ mexpr)) = do
    exprType <- maybe (return $ Annot (pos typ) $ simpleType Nil) typeCheckExpr mexpr
    exprType `shouldBeWithin` typ
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot r (Assignment var val)) = do
    typeOfVar <- typeCheckLValue var
    exprType <- typeCheckExpr val
    exprType `shouldBeWithin` typeOfVar
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot _ (Expr x)) = typeCheckExpr x

typeCheckLValue :: LValue Resolved -> T (Annot Type)
typeCheckLValue (LValueVar var)                      = typeCheckLIdent var
typeCheckLValue (LValueField expr field@(Annot r _)) = typeCheckExpr (Annot r (FieldAccess expr field))

typeCheckExpr :: Expr Resolved -> T (Annot Type)
typeCheckExpr (Annot _ (Variable x)) = typeCheckLIdent x
typeCheckExpr (Annot r (FuncCall f args)) = do
    func <- typeCheckLIdent f
    args' <- traverse typeCheckExpr args
    lift $ left (\(s,l) -> NotWithin (Annot (pos func) s) (Annot (foldMap pos args) l)) $ right (Annot r) $ validateFuncCall (unAnnot func) (unAnnot <$> args')
typeCheckExpr (Annot r (BoolLiteral x)) = return $ Annot r (constBool x)
typeCheckExpr (Annot r (NumLiteral x)) = return $ Annot r (constNumber x)
typeCheckExpr (Annot r (StringLiteral x)) = return $ Annot r (constString x)
typeCheckExpr (Annot r (ArrayExpr xs)) = do
    types <- traverse typeCheckExpr xs
    return $ Annot r $ array $ mconcat $ unAnnot <$> types
typeCheckExpr (Annot range (BinOp o@(Annot _ op) l r)) = do
    (Annot _ l') <- typeCheckExpr l
    (Annot _ r') <- typeCheckExpr r
    Annot range <$> lift (typeCheckBinOp l' r')
    where
        typeCheckBinOp :: Type -> Type -> Either TypeCheckErr Type
        typeCheckBinOp l r
            | op `elem` [EqOp, NotEqOp] = return (simpleType Bool)
            | op `elem` [SubOp, MulOp, DivOp, ModOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number)] l r
            | op `elem` [AndOp, OrOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Bool,Bool,Bool)] l r
            | op == AddOp = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number), (String,String,String)] l r
            | op `elem` [LessOp, GreaterOp, LessEqualOp, GreaterEqualOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Bool)] l r
            | otherwise = error $ "Not a valid BinOp: " ++ show op
typeCheckExpr (Annot range (UnOp o@(Annot _ NegOp) x)) = fmap (Annot range) $
    typeCheckExpr x
    >>= (lift . left (InvalidUnOp o) . validateUnOp [(Number, Number)] . unAnnot)
typeCheckExpr (Annot range (UnOp o@(Annot _ NotOp) x)) = fmap (Annot range) $
    typeCheckExpr x
    >>= (lift . left (InvalidUnOp o) . validateUnOp [(Bool, Bool)] . unAnnot)
typeCheckExpr (Annot _ (Cast typ x)) = do
    _ <- typeCheckExpr x
    return typ
typeCheckExpr (Annot r (Tuple xs)) = Annot r . tuple . map unAnnot <$> traverse typeCheckExpr xs
typeCheckExpr (Annot r (IfStatement cond ifT)) = do
    typeOfCond <- typeCheckExpr cond
    typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    case ifT of
        ThenDo thn mElse -> do
            thnTyp <- typeCheckBlock thn
            mElseTyp <- traverse typeCheckBlock mElse
            return $ maybe thnTyp (thnTyp<>) mElseTyp
        ThenExitWith exitBlock -> do
            exitWithTyp <- typeCheckBlock exitBlock
            f <- curFacts
            exitWith f $ unAnnot exitWithTyp
            return (Annot r $ simpleType Nil) -- If a value is passed back then the exitWith was not hit
typeCheckExpr (Annot _ (WhileLoop cond stmt)) = do
    typeOfCond <- typeCheckExpr cond
    typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    typeCheckBlock stmt
typeCheckExpr (Annot r NilLit) = return (Annot r $ simpleType Nil)
typeCheckExpr (Annot r (FieldAccess expr field)) = do
    exprType <- typeCheckExpr expr
    let fieldType = lookupField (unVarName $ unAnnot field) $ unAnnot exprType
    case fieldType of
        Nothing  -> lift $ Left $ NoField field exprType
        (Just x) -> return $ Annot r x

foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f = foldr (\x acc -> (<>) <$> f x <*> acc) (pure mempty)

typeCheckBlock :: [Statement Resolved] -> T (Annot Type)
typeCheckBlock stmts
    = (\(exitWith, x) -> (exitWith<>) <$> fromMaybe mempty (getLast x))
    <$> block (foldMapM (fmap (Last . Just) . typeCheckStmt) stmts)

typeCheckLIdent :: Annot (Ident Resolved) -> T (Annot Type)
typeCheckLIdent (Annot r (Ident i args)) = lift $
    fmap (Annot r) $ mapLeft NotFound $ resolveGenericType r (lIdentType i) (unAnnot <$> args)
