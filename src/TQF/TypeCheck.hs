{-# LANGUAGE RecordWildCards #-}
module TQF.TypeCheck
    ( TypeCheckErr(..)
    , typeCheck
    ) where

import           Control.Arrow
import           Control.Error.Util         (note)
import           Control.Monad              (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Either.Extra
import           Data.Foldable              (fold, foldrM, traverse_)
import           Data.Functor
import           Data.Maybe
import           Data.String.Pretty
import           Safe                       (lastMay)
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve                (resolveGenericType)
import           TQF.Type

data TypeCheckErr = NotWithin (Annot (Type' String)) (Annot (Type' String))
     | NoField (Annot VarName) (Annot (Type' String))
     | InvalidBinOp (Annot BinaryOperator) (Type' String) (Type' String)
     | InvalidUnOp (Annot UnaryOperator) (Type' String)
     | ExpectedCode Range
     | NotFound (Annot UIdent)
     deriving (Show, Eq)

instance Pretty TypeCheckErr where
    prettyPrint (NotWithin l r) = "type\n" ++ prettyPrint l ++ "\nis not within type\n" ++ prettyPrint r
    prettyPrint (NoField name typ) = "type\n" ++ prettyPrint typ ++ "\n does not have a field " ++ prettyPrint name
    prettyPrint (InvalidBinOp op l r) = "Invalid arguments to binary operator " ++ prettyPrint op ++ ": " ++ prettyPrint l ++ " and " ++ prettyPrint r
    prettyPrint (InvalidUnOp op x) = "Invalid argument to unary operator " ++ prettyPrint op ++ ": " ++ prettyPrint x
    prettyPrint (ExpectedCode r) = "Expected code " ++ prettyPrint r
    prettyPrint (NotFound x) = "Not found: " ++ prettyPrint x

shouldBeWithin :: Annot (Type' String) -> Annot (Type' String) -> Either TypeCheckErr ()
shouldBeWithin s@(Annot _ small) l@(Annot _ large) = if small `isWithin` large then return () else Left $ NotWithin s l

typeCheck :: Module Resolved -> Either TypeCheckErr ()
typeCheck Module{..} = mapM_ typeCheckDeclaration moduleDeclarations

typeCheckDeclaration :: Declaration Resolved -> Either TypeCheckErr ()
typeCheckDeclaration (Annot _ FunctionDecl{..}) = do
    blockType <- typeCheckBlock functionContent
    blockType `shouldBeWithin` functionType
typeCheckDeclaration (Annot _ VariableDecl{..}) = return ()
typeCheckDeclaration (Annot _ TypeDecl{..}) = return ()
typeCheckDeclaration (Annot _ CommandDecl{..}) = return ()
typeCheckDeclaration (Annot _ ExternalFunctionDecl{..}) = return ()
typeCheckDeclaration (Annot _ ExternalVariableDecl{..}) = return ()

typeCheckStmt :: Statement Resolved -> WriterT (Maybe (Annot Type)) (Either TypeCheckErr) (Annot Type)
typeCheckStmt (Annot r (VariableDeclaration typ _ mexpr)) = do
    exprType <- maybe (return $ Annot (pos typ) $ simpleType Nil) typeCheckExpr mexpr
    lift $ exprType `shouldBeWithin` typ
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot r (Assignment var val)) = do
    typeOfVar <- lift $ typeCheckLIdent var
    exprType <- typeCheckExpr val
    lift $ exprType `shouldBeWithin` typeOfVar
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot r (Expr x)) = typeCheckExpr x

typeCheckExpr :: Expr Resolved -> WriterT (Maybe (Annot Type)) (Either TypeCheckErr) (Annot Type)
typeCheckExpr (Annot r (Variable x)) = lift $ typeCheckLIdent x
typeCheckExpr (Annot r (FuncCall f args)) = do
    func <- lift $ typeCheckLIdent f
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
    case ifT of
        ThenDo thn mElse -> do
            thnTyp <- lift $ typeCheckBlock thn
            mElseTyp <- lift $ traverse typeCheckBlock mElse
            return $ maybe thnTyp (thnTyp<>) mElseTyp
        ThenExitWith exitWith -> do
            exitWithTyp <- lift $ typeCheckBlock exitWith
            tell $ Just exitWithTyp
            return (Annot r $ simpleType Nil) -- If a value is passed back then the exitWith was not hit
    lift $ typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    lift $ fold <$> mapM typeCheckBlock ifT
typeCheckExpr (Annot r (WhileLoop cond stmt)) = do
    typeOfCond <- typeCheckExpr cond
    lift $ typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    lift $ typeCheckBlock stmt
typeCheckExpr (Annot r NilLit) = return (Annot r $ simpleType Nil)

typeCheckBlock :: [Statement Resolved] -> Either TypeCheckErr (Annot Type)
typeCheckBlock = fmap f . runWriterT . traverse typeCheckStmt
    where
        f :: ([Annot Type], Maybe (Annot Type)) -> Annot Type
        f (typs, mExitWith) =
            let mExitWithTyp = mExitWith
                retTyp = fromMaybe (Annot NoPlace $ simpleType Nil) $ lastMay typs
            in maybe retTyp (retTyp<>) mExitWithTyp

typeCheckLIdent :: Annot (Ident Resolved) -> Either TypeCheckErr (Annot Type)
typeCheckLIdent (Annot r (Ident i args fields)) = do
    initType <- mapLeft NotFound $ resolveGenericType r (lIdentType i) (unAnnot <$> args)
    Annot r <$> foldrM go initType fields
    --Annot r <$> foldrM go (lIdentType initial) fields
    where
        go :: Annot VarName -> Type -> Either TypeCheckErr Type
        go field x = note (NoField field (Annot r x)) $ lookupField (unVarName $ unAnnot field) x
