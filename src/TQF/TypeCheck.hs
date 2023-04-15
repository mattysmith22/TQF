{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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
import           TQF.TypeCheck.Narrowing
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

        runNarrowing
            :: Maybe Bool
            -> NarrowableExpr
            -> T ()
        runNarrowing Nothing _         = return ()
        runNarrowing (Just guess) expr = sequence_ $ catMaybes ((\n -> n guess expr) <$> narrowingRules)

        zipNewInfo = zipWith (\a b -> (,unAnnot b) . fst <$> a)

        tc' :: ExprF Type (Ident Resolved, T MGType) ([Statement Resolved], T Type) (Expr_ Resolved, Maybe Bool -> T MGType) -> Maybe Bool -> T MGType
        tc' (VariableF c) mGuess = do
            varTyp <- snd (unAnnot c)
            runNarrowing mGuess (VariableF ((,varTyp) . fst <$> c))
            return varTyp
        tc' (FuncCallF func args) mGuess = do
            genArgs' <- traverse (runExprTc Nothing) args
            args' <-traverse enforceConcrete genArgs'
            func' <- snd (unAnnot func) Nothing
            concFuncType <- case func' of
                    (Generic func'@(GenericType args genTyp)) -> do
                        let inferredType = code (unAnnot <$> args') bottom
                            inference = runInference genTyp inferredType
                        inferredParams <- lift $ note (CouldntInferType $ Annot (pos func) func')
                                $ mapM (`Map.lookup` inference) args
                        lift $ left NotFound $ resolveGenericType (pos func) func' inferredParams
                    (Concrete func') -> return func'
            retType <- lift
                    $ left (\(s,l) -> NotWithin (Annot (pos func) s) (Annot (foldMap pos args) l))
                    $ Concrete
                    <$> validateFuncCall concFuncType (unAnnot <$> args')
            runNarrowing mGuess (FuncCallF ((,func') . fst <$> func) (zipNewInfo args genArgs'))
            return retType
        tc' (BoolLiteralF x) mGuess = do
            runNarrowing mGuess (BoolLiteralF x)
            return $ Concrete $  constBool x
        tc' (NumLiteralF x) mGuess = do
            runNarrowing mGuess (NumLiteralF x)
            return $ Concrete $ constNumber x
        tc' (StringLiteralF x) mGuess = do
            runNarrowing mGuess (StringLiteralF x)
            return $ Concrete $ constString x
        tc' (ArrayExprF xs) mGuess = do
            genTypes <- traverse (runExprTc Nothing) xs
            types <- traverse enforceConcrete genTypes
            runNarrowing mGuess (ArrayExprF $ zipNewInfo xs genTypes)
            return $ Concrete $ array $ mconcat $ unAnnot <$> types

        tc' (BinOpF o@(Annot _ AndOp) l r) (Just True) = do
            (Annot _ l') <- enforceConcrete =<< runExprTc (Just True) l
            (Annot _ r') <- enforceConcrete =<< runExprTc (Just True) r
            Concrete <$> lift (typeCheckBinOp o l' r')
        tc' (BinOpF o@(Annot _ AndOp) l r) (Just False)
            = Concrete . uncurry (<>) <$> oneOf
                (do
                    (Annot _ l') <- enforceConcrete =<< runExprTc (Just True) l
                    (Annot _ r') <- enforceConcrete =<< runExprTc (Just False) r
                    lift (typeCheckBinOp o l' r'))
                (runCondTc False l)
        tc' (BinOpF o@(Annot _ AndOp) l r) Nothing
            = Concrete . uncurry (<>) <$> oneOf
                (do
                    (Annot _ l') <- enforceConcrete =<< runExprTc (Just True) l
                    (Annot _ r') <- enforceConcrete =<< runExprTc Nothing r
                    lift (typeCheckBinOp o l' r'))
                (runCondTc False l)
        tc' (BinOpF o@(Annot _ OrOp) l r) (Just False) = do
            (Annot _ l') <- enforceConcrete =<< runExprTc (Just False) l
            (Annot _ r') <- enforceConcrete =<< runExprTc (Just False) r
            Concrete <$> lift (typeCheckBinOp o l' r')
        tc' (BinOpF o@(Annot _ OrOp) l r) (Just True)
            = Concrete . uncurry (<>) <$> oneOf
                (do
                    (Annot _ l') <- enforceConcrete =<< runExprTc (Just False) l
                    (Annot _ r') <- enforceConcrete =<< runExprTc (Just True) r
                    lift (typeCheckBinOp o l' r'))
                (runCondTc True l)
        tc' (BinOpF o@(Annot _ OrOp) l r) Nothing
            = Concrete . uncurry (<>) <$> oneOf
                (do
                    (Annot _ l') <- enforceConcrete =<< runExprTc (Just False) l
                    (Annot _ r') <- enforceConcrete =<< runExprTc Nothing r
                    lift (typeCheckBinOp o l' r'))
                (runCondTc True l)

        tc' (BinOpF o l r) mGuess = do
            genL <- runExprTc Nothing l
            genR <- runExprTc Nothing r
            (Annot _ l') <- enforceConcrete genL
            (Annot _ r') <- enforceConcrete genR
            runNarrowing mGuess (BinOpF o ((,unAnnot genL) . fst <$> l) ((,unAnnot genR) . fst <$> r))
            Concrete <$> lift (typeCheckBinOp o l' r')
        tc' (UnOpF o@(Annot _ NegOp) x) mGuess = do
            genTyp <- runExprTc Nothing x
            retTyp <- enforceConcrete genTyp >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Number, Number)] . unAnnot)
            runNarrowing mGuess (UnOpF o ((,unAnnot genTyp) . fst <$> x))
            return retTyp
        tc' (UnOpF o@(Annot _ NotOp) x) mGuess = do
            genTyp <- runExprTc (not <$> mGuess) x
            retTyp <- enforceConcrete genTyp >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Bool, Bool)] . unAnnot)
            runNarrowing mGuess (UnOpF o ((,unAnnot genTyp) . fst <$> x))
            return retTyp

        tc' (CastF typ x) mGuess = do
            genTyp <- runExprTc Nothing x
            runNarrowing mGuess (CastF typ ((,unAnnot genTyp). fst <$> x))
            return $ Concrete $ unAnnot typ
        tc' (TupleF xs) mGuess = do
            genTyps <- traverse (runExprTc Nothing) xs
            retTyp <- Concrete
                    . tuple
                    . map unAnnot
                    <$> traverse enforceConcrete genTyps
            runNarrowing mGuess (TupleF $ zipNewInfo xs genTyps)
            return retTyp
        tc' (IfStatementF cond ifT) mGuess =
            case ifT of
                (ThenDo thn mElse) -> do
                    retTyps <- oneOf
                        (runCondTc True cond >> snd thn)
                        (runCondTc False cond >> traverse snd mElse)
                    runNarrowing mGuess (IfStatementF ((,Concrete $ simpleType Bool) . fst <$> cond)
                        $ ThenDo
                            (fst thn,Concrete $ fst retTyps)
                            ((\l r -> (fst l,Concrete r)) <$> mElse <*> snd retTyps))
                    return $ Concrete $ (\(l,r) -> l <> fromMaybe (simpleType Nil) r) retTyps
                (ThenExitWith xit) -> do
                    retTyps <- oneOf
                        (runCondTc True cond >> exitWith (snd xit) >> return mempty)
                        (runCondTc False cond >> return (simpleType Nil))-- If a value is passed back then the exitWith was not hit
                    return $ Concrete $ uncurry (<>) retTyps
        tc' (WhileLoopF cond stmt) mGuess = do
            genTypOfCond <- runExprTc Nothing cond
            typeOfCond <- enforceConcrete genTypOfCond
            typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
            typeOfStmt <- Concrete <$> snd stmt
            runNarrowing mGuess (WhileLoopF ((,unAnnot genTypOfCond). fst <$>cond) (fst stmt, typeOfStmt))
            return typeOfStmt

        tc' NilLitF mGuess = do
            runNarrowing mGuess NilLitF
            return (Concrete $ simpleType Nil)
        tc' (FieldAccessF expr field) mGuess = do
            exprGenType <- runExprTc Nothing expr
            exprType <- enforceConcrete exprGenType
            let fieldType = lookupField (unVarName $ unAnnot field) $ unAnnot exprType
            case fieldType of
                Nothing  -> lift $ Left $ NoField field exprType
                (Just x) -> do
                    runNarrowing mGuess (FieldAccessF ((,unAnnot exprGenType).fst<$>expr) field)
                    return $ Concrete x

        runCondTc x expr
            = runExprTc (Just x) expr
            >>= enforceConcrete
            >>= (`shouldBeWithin` Annot (pos expr) (simpleType Bool))
            >> return (simpleType Bool)

typeCheckBinOp :: Annot BinaryOperator -> Type -> Type -> Either TypeCheckErr Type
typeCheckBinOp o l r
    | op `elem` [EqOp, NotEqOp] = return (simpleType Bool)
    | op `elem` [SubOp, MulOp, DivOp, ModOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number)] l r
    | op `elem` [AndOp, OrOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Bool,Bool,Bool)] l r
    | op == AddOp = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Number), (String,String,String)] l r
    | op `elem` [LessOp, GreaterOp, LessEqualOp, GreaterEqualOp] = left (uncurry $ InvalidBinOp o) $ validateBinOp [(Number,Number,Bool)] l r
    | otherwise = error $ "Not a valid BinOp: " ++ show op
    where op = unAnnot o

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
