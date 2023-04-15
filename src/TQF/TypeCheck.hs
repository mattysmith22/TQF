{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TQF.TypeCheck
    ( TypeCheckErr(..)
    , typeCheck
    ) where

import           Control.Arrow
import           Control.Error             (note)
import           Control.Monad             ((<=<))
import           Control.Monad.Trans.Class
import           Data.Either.Extra
import           Data.Functor.Foldable     (Base, para)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve
import           TQF.Type
import           TQF.TypeCheck.Facts
import           TQF.TypeCheck.Monad
import           TQF.TypeCheck.Types

shouldBeWithin :: Annot (Type' String) -> Annot (Type' String) -> T ()
shouldBeWithin s@(Annot _ small) l@(Annot _ large) = if small `isWithin` large then return () else lift $ Left $ NotWithin s l

typeCheck :: Module Resolved -> Either TypeCheckErr ()
typeCheck Module{..} = runTypeCheck $ mapM_ typeCheckDeclaration moduleDeclarations

typeCheckDeclaration :: Declaration Resolved -> T ()
typeCheckDeclaration (Annot r FunctionDecl{..}) = do
    blockType <- typeCheckBlock functionContent
    Annot r blockType `shouldBeWithin` functionType
typeCheckDeclaration (Annot _ VariableDecl{}) = return ()
typeCheckDeclaration (Annot _ TypeDecl{}) = return ()
typeCheckDeclaration (Annot _ CommandDecl{}) = return ()
typeCheckDeclaration (Annot _ ExternalFunctionDecl{}) = return ()
typeCheckDeclaration (Annot _ ExternalVariableDecl{}) = return ()

typeCheckStmt :: Statement Resolved -> T (Annot Type)
typeCheckStmt (Annot r (VariableDeclaration typ _ mexpr)) = do
    exprType <- maybe (return $ Annot (pos typ) $ simpleType Nil) (enforceConcrete <=< flip typeCheckExpr Nothing) mexpr
    exprType `shouldBeWithin` typ
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot r (Assignment var val)) = do
    typeOfVar <- typeCheckLValue var
    exprType <- enforceConcrete =<< typeCheckExpr val Nothing
    exprType `shouldBeWithin` typeOfVar
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot _ (Expr x)) = enforceConcrete =<< typeCheckExpr x Nothing

typeCheckLValue :: LValue Resolved -> T (Annot Type)
typeCheckLValue (LValueVar var)                      = enforceConcrete =<< traverse typeCheckLIdent var
typeCheckLValue (LValueField expr field@(Annot r _)) = enforceConcrete =<< typeCheckExpr (Annot r (FieldAccess expr field)) Nothing

enforceConcrete :: Annot MGType -> T (Annot Type)
enforceConcrete (Annot r (Concrete x)) = return $ Annot r x
enforceConcrete (Annot r (Generic x))  = lift $ Left $ CouldntInferType $ Annot r x

typeCheckExpr :: Expr Resolved -> Maybe Bool -> T (Annot MGType)
typeCheckExpr (Annot r expr_) = fmap (Annot r) <$> para tc expr_
    where
        tc :: Base (Expr_ Resolved) (Expr_ Resolved, Maybe Bool -> T MGType) -> Maybe Bool -> T MGType
        tc = tc' . mapIdent (id&&&typeCheckLIdent) . mapBlock (id&&&typeCheckBlock)

        runExprTc x = traverse (($x) . snd)

        tc' :: ExprF Type (Ident Resolved, T MGType) ([Statement Resolved], T Type) (Expr_ Resolved, Maybe Bool -> T MGType) -> Maybe Bool -> T MGType
        tc' (VariableF c) _ = snd $ unAnnot c
        tc' (FuncCallF func args) _ = do
            args' <- traverse (enforceConcrete =<<) (runExprTc Nothing <$> args)
            func' <- snd (unAnnot func) Nothing
            concFuncType <- case func' of
                    (Generic func'@(GenericType args genTyp)) -> do
                        let inferredType = code (unAnnot <$> args') bottom
                            inference = runInference genTyp inferredType
                        inferredParams <- lift $ note (CouldntInferType $ Annot (pos func) func')
                                $ mapM (`Map.lookup` inference) args
                        lift $ left NotFound $ resolveGenericType (pos func) func' inferredParams
                    (Concrete func') -> return func'
            lift
                $ left (\(s,l) -> NotWithin (Annot (pos func) s) (Annot (foldMap pos args) l))
                $ Concrete
                <$> validateFuncCall concFuncType (unAnnot <$> args')
        tc' (BoolLiteralF x) _ = return $ Concrete $ constBool x
        tc' (NumLiteralF x) _ = return $ Concrete $ constNumber x
        tc' (StringLiteralF x) _ = return $ Concrete $ constString x
        tc' (ArrayExprF xs) _ = do
            types <- traverse (enforceConcrete =<<) (runExprTc Nothing <$> xs)
            return $ Concrete $ array $ mconcat $ unAnnot <$> types
        tc' (BinOpF o@(Annot _ op) l r) _ = do
            (Annot _ l') <- enforceConcrete =<< runExprTc Nothing l
            (Annot _ r') <- enforceConcrete =<< runExprTc Nothing r
            Concrete <$> lift (typeCheckBinOp l' r')
            where
            typeCheckBinOp :: Type -> Type -> Either TypeCheckErr Type
            typeCheckBinOp l r
                | op `elem` [EqOp, NotEqOp] = return (simpleType Bool)
                | op `elem` [SubOp, MulOp, DivOp, ModOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number)] l r
                | op `elem` [AndOp, OrOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Bool,Bool,Bool)] l r
                | op == AddOp = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number), (String,String,String)] l r
                | op `elem` [LessOp, GreaterOp, LessEqualOp, GreaterEqualOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Bool)] l r
                | otherwise = error $ "Not a valid BinOp: " ++ show op
        tc' (UnOpF o@(Annot _ NegOp) x) _ =
            runExprTc Nothing x >>= enforceConcrete
            >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Number, Number)] . unAnnot)
        tc' (UnOpF o@(Annot _ NotOp) x) _ =
            runExprTc Nothing x >>= enforceConcrete
            >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Bool, Bool)] . unAnnot)
        tc' (CastF typ _) _ =
            return $ Concrete $ unAnnot typ
        tc' (TupleF xs) _
            = Concrete
            . tuple
            . map unAnnot
            <$> traverse (enforceConcrete=<<) (runExprTc Nothing <$> xs)
        tc' (IfStatementF cond ifT) _ = do
            cond' <- enforceConcrete =<< runExprTc Nothing cond
            cond' `shouldBeWithin` Annot (pos cond) (simpleType Bool)
            case ifT of
                (ThenDo thn mElse) ->
                    Concrete . uncurry (<>) <$>
                    oneOf
                        (snd thn)
                        (maybe (return $ simpleType Nil) snd mElse)
                (ThenExitWith xit) ->
                    Concrete . uncurry (<>) <$>
                    oneOf
                        (exitWith (snd xit) >> return mempty)
                        (return $ simpleType Nil)-- If a value is passed back then the exitWith was not hit
        tc' (WhileLoopF cond stmt) _ = do
            typeOfCond <- enforceConcrete =<< runExprTc Nothing cond
            typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
            Concrete <$> snd stmt
        tc' NilLitF _ = return (Concrete $ simpleType Nil)
        tc' (FieldAccessF expr field) _ = do
            exprType <- enforceConcrete =<< runExprTc Nothing expr
            let fieldType = lookupField (unVarName $ unAnnot field) $ unAnnot exprType
            case fieldType of
                Nothing  -> lift $ Left $ NoField field exprType
                (Just x) -> return $ Concrete x

foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f = foldr (\x acc -> (<>) <$> f x <*> acc) (pure mempty)

typeCheckBlock :: [Statement Resolved] -> T Type
typeCheckBlock stmts
    = unAnnot . (\(exitWith, x) -> (exitWith<>) <$> fromMaybe mempty (getLast x))
    <$> block (foldMapM (fmap (Last . Just) . typeCheckStmt) stmts)

typeCheckLIdent :: Ident Resolved -> T MGType
typeCheckLIdent i = do
    facts <- curFacts
    let i' = resolveIdentType facts i
    case (identTypeArgs i', genTypeArgs $ lIdentType $ identName i') of
        ([], _:_) -> return $ Generic $ lIdentType $ identName i'
        _ -> lift $ fmap Concrete $ mapLeft NotFound $ resolveGenericType NoPlace (lIdentType $ identName i') (unAnnot <$> identTypeArgs i')
