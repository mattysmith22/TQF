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
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String.Pretty
import           Debug.Trace
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve               (resolveGenericType)
import           TQF.Type
import           TQF.TypeCheck.Facts
import           TQF.TypeCheck.Monad

data TypeCheckErr = NotWithin (Annot (Type' String)) (Annot (Type' String))
     | NoField (Annot VarName) (Annot (Type' String))
     | InvalidBinOp (Annot BinaryOperator) (Type' String) (Type' String)
     | InvalidUnOp (Annot UnaryOperator) (Type' String)
     | ExpectedCode Range
     | NotFound (Annot UIdent)
     | CouldntInferType (Annot GenericType)
     deriving (Show, Eq)

type T a = TypeCheck Facts (Type' String) (Either TypeCheckErr) a

data MGType
    = Generic GenericType
    | Concrete Type
    deriving (Show, Eq)

instance Pretty TypeCheckErr where
    prettyPrint (NotWithin l r) = "type\n" ++ prettyPrint l ++ "\nis not within type\n" ++ prettyPrint r
    prettyPrint (NoField name typ) = "type\n" ++ prettyPrint typ ++ "\n does not have a field " ++ prettyPrint name
    prettyPrint (InvalidBinOp op l r) = "Invalid arguments to binary operator " ++ prettyPrint op ++ ": " ++ prettyPrint l ++ " and " ++ prettyPrint r
    prettyPrint (InvalidUnOp op x) = "Invalid argument to unary operator " ++ prettyPrint op ++ ": " ++ prettyPrint x
    prettyPrint (ExpectedCode r) = "Expected code " ++ prettyPrint r
    prettyPrint (NotFound x) = "Not found: " ++ prettyPrint x
    prettyPrint (CouldntInferType x) = "Couldn't infer type at " ++ prettyPrint (pos x)

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
    exprType <- maybe (return $ Annot (pos typ) $ simpleType Nil) (enforceConcrete <=< typeCheckExpr) mexpr
    exprType `shouldBeWithin` typ
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot r (Assignment var val)) = do
    typeOfVar <- typeCheckLValue var
    exprType <- enforceConcrete =<< typeCheckExpr val
    exprType `shouldBeWithin` typeOfVar
    return $ Annot r $ simpleType Nil
typeCheckStmt (Annot _ (Expr x)) = enforceConcrete =<< typeCheckExpr x

typeCheckLValue :: LValue Resolved -> T (Annot Type)
typeCheckLValue (LValueVar var)                      = enforceConcrete =<< typeCheckLIdent var
typeCheckLValue (LValueField expr field@(Annot r _)) = enforceConcrete =<< typeCheckExpr (Annot r (FieldAccess expr field))

enforceConcrete :: Annot MGType -> T (Annot Type)
enforceConcrete (Annot r (Concrete x)) = return $ Annot r x
enforceConcrete (Annot r (Generic x))  = lift $ Left $ CouldntInferType $ Annot r x

typeCheckExpr :: Expr Resolved -> T (Annot MGType)
typeCheckExpr (Annot _ (Variable x)) = typeCheckLIdent x
typeCheckExpr (Annot r (FuncCall f args)) = do
    func <- typeCheckExpr f
    args' <- traverse (enforceConcrete <=< typeCheckExpr) args
    concFuncType <- case unAnnot func of
            (Generic func'@(GenericType args genTyp)) -> do
                let inferredType = code (unAnnot <$> args') bottom
                    inference = runInference genTyp inferredType
                inferredParams <- lift $ note (trace (unlines [show func,show args,show inferredType,show inference]) (CouldntInferType $ Annot (pos func) func'))
                        $ mapM (`Map.lookup` inference) args
                lift $ left NotFound $ resolveGenericType (pos func) func' inferredParams
            (Concrete func') -> return func'
    lift
        $ left (\(s,l) -> NotWithin (Annot (pos func) s) (Annot (foldMap pos args) l))
        $ Annot r . Concrete
        <$> validateFuncCall concFuncType (unAnnot <$> args')
typeCheckExpr (Annot r (BoolLiteral x)) = return $ Annot r $ Concrete $ constBool x
typeCheckExpr (Annot r (NumLiteral x)) = return $ Annot r $ Concrete $ constNumber x
typeCheckExpr (Annot r (StringLiteral x)) = return $ Annot r $ Concrete $ constString x
typeCheckExpr (Annot r (ArrayExpr xs)) = do
    types <- traverse (enforceConcrete <=< typeCheckExpr) xs
    return $ Annot r $ Concrete $ array $ mconcat $ unAnnot <$> types
typeCheckExpr (Annot range (BinOp o@(Annot _ op) l r)) = do
    (Annot _ l') <- enforceConcrete =<< typeCheckExpr l
    (Annot _ r') <- enforceConcrete =<< typeCheckExpr r
    Annot range . Concrete <$> lift (typeCheckBinOp l' r')
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
    >>= enforceConcrete
    >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Number, Number)] . unAnnot)
typeCheckExpr (Annot range (UnOp o@(Annot _ NotOp) x)) = fmap (Annot range) $
    typeCheckExpr x
    >>= enforceConcrete
    >>= (lift . fmap Concrete . left (InvalidUnOp o) . validateUnOp [(Bool, Bool)] . unAnnot)
typeCheckExpr (Annot _ (Cast typ x)) = do
    _ <- typeCheckExpr x
    return $ Concrete <$> typ
typeCheckExpr (Annot r (Tuple xs)) = Annot r . Concrete . tuple . map unAnnot <$> traverse (enforceConcrete <=< typeCheckExpr) xs
typeCheckExpr (Annot r (IfStatement cond ifT)) = do
    typeOfCond <- enforceConcrete =<< typeCheckExpr cond
    typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    case ifT of
        ThenDo thn mElse -> do
            thnTyp <- typeCheckBlock thn
            mElseTyp <- traverse typeCheckBlock mElse
            return $ Concrete <$> maybe thnTyp (thnTyp<>) mElseTyp
        ThenExitWith exitBlock -> do
            exitWithTyp <- typeCheckBlock exitBlock
            f <- curFacts
            exitWith f $ unAnnot exitWithTyp
            return (Annot r $ Concrete $ simpleType Nil) -- If a value is passed back then the exitWith was not hit
typeCheckExpr (Annot _ (WhileLoop cond stmt)) = do
    typeOfCond <- enforceConcrete =<< typeCheckExpr cond
    typeOfCond `shouldBeWithin` Annot (pos cond) (simpleType Bool)
    fmap Concrete <$> typeCheckBlock stmt
typeCheckExpr (Annot r NilLit) = return (Annot r $ Concrete $ simpleType Nil)
typeCheckExpr (Annot r (FieldAccess expr field)) = do
    exprType <- enforceConcrete =<< typeCheckExpr expr
    let fieldType = lookupField (unVarName $ unAnnot field) $ unAnnot exprType
    case fieldType of
        Nothing  -> lift $ Left $ NoField field exprType
        (Just x) -> return $ Annot r $ Concrete x

foldMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f = foldr (\x acc -> (<>) <$> f x <*> acc) (pure mempty)

typeCheckBlock :: [Statement Resolved] -> T (Annot Type)
typeCheckBlock stmts
    = (\(exitWith, x) -> (exitWith<>) <$> fromMaybe mempty (getLast x))
    <$> block (foldMapM (fmap (Last . Just) . typeCheckStmt) stmts)

typeCheckLIdent :: Annot (Ident Resolved) -> T (Annot MGType)
typeCheckLIdent (Annot r i) = do
    facts <- curFacts
    let i' = resolveIdentType facts i
    case (identTypeArgs i', genTypeArgs $ lIdentType $ identName i') of
        ([], _:_) -> return $ Annot r $ Generic $ lIdentType $ identName i'
        _ -> lift $ fmap (Annot r . Concrete) $ mapLeft NotFound $ resolveGenericType r (lIdentType $ identName i') (unAnnot <$> identTypeArgs i')
