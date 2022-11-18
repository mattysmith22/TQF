{-# LANGUAGE RecordWildCards #-}
module TQF.TypeCheck
    ( TypeCheckErr(..)
    , typeCheck
    ) where

import TQF.AST
import TQF.AST.Annotated
import TQF.Type
import Control.Error.Util (note)
import Control.Monad (when)
import Control.Arrow
import Safe (lastMay)
import Data.Foldable (foldrM, traverse_)
import Data.String.Pretty

data TypeCheckErr = NotWithin (Annot Type) (Annot Type)
     | NoField VarName (Annot Type)
     | BadTopLevel Range
     | InvalidBinOp (Annot BinaryOperator) Type Type
     | InvalidUnOp (Annot UnaryOperator) Type
     deriving (Show, Eq)

instance Pretty TypeCheckErr where
    prettyPrint (NotWithin l r) = "type\n" ++ prettyPrint l ++ "\nis not within type\n" ++ prettyPrint r
    prettyPrint (NoField name typ) = "type\n" ++ prettyPrint typ ++ "\n does not have a field " ++ prettyPrint name
    prettyPrint (BadTopLevel r) = "Bad top level statement at " ++ prettyPrint r
    prettyPrint (InvalidBinOp op l r) = "Invalid arguments to binary operator " ++ prettyPrint op ++ ": " ++ prettyPrint l ++ " and " ++ prettyPrint r
    prettyPrint (InvalidUnOp op x) = "Invalid argument to unary operator " ++ prettyPrint op ++ ": " ++ prettyPrint x

shouldBeWithin :: Annot Type -> Annot Type -> Either TypeCheckErr ()
shouldBeWithin s@(Annot _ small) l@(Annot _ large) = if small `isWithin` large then return () else Left $ NotWithin s l

typeCheck :: Module Resolved -> Either TypeCheckErr ()
typeCheck Module{..} = mapM_ typeCheckDeclaration moduleDeclarations

typeCheckDeclaration :: Declaration Resolved -> Either TypeCheckErr ()
typeCheckDeclaration (Annot _ FunctionDecl{..}) = typeCheckStatement (Env functionType True) functionContent
typeCheckDeclaration (Annot _ VariableDecl{..}) = return ()
typeCheckDeclaration (Annot _ TypeDecl{..}) = return ()
typeCheckDeclaration (Annot _ CommandDecl{..}) = return ()
typeCheckDeclaration (Annot _ ExternalFunctionDecl{..}) = return ()
typeCheckDeclaration (Annot _ ExternalVariableDecl{..}) = return ()

data Env = Env
    { returnType :: Annot Type
    , topLevelStatement :: Bool
    }

shouldNotBeTopLevelStatement :: Env -> Range -> Either TypeCheckErr ()
shouldNotBeTopLevelStatement Env{..} r = when topLevelStatement (Left $ BadTopLevel r)

notTopLevel :: Env -> Env
notTopLevel e = e {topLevelStatement = False}

typeCheckStatement :: Env -> Statement Resolved -> Either TypeCheckErr ()
typeCheckStatement env (Annot r x) = typeCheckStatement' env r x

typeCheckStatement' :: Env -> Range -> Statement_ Resolved -> Either TypeCheckErr ()
typeCheckStatement' env r (CodeBlock stmts) = do
    traverse_ (typeCheckStatement (notTopLevel env)) stmts
    when (topLevelStatement env) $ case lastMay stmts of
            Nothing -> returnType env `shouldBeWithin` Annot r (simpleType Nil)
            (Just (Annot _ (Return _))) -> return ()
            (Just _) -> returnType env `shouldBeWithin` Annot r (simpleType Nil)
typeCheckStatement' env r VariableDeclaration{..} = do
    shouldNotBeTopLevelStatement env r
    exprType <- maybe (return $ Annot (pos varDeclType) $ simpleType Nil) typeCheckExpr varDeclValue
    exprType `shouldBeWithin` varDeclType
typeCheckStatement' env r FunctionCall{..} = do
    shouldNotBeTopLevelStatement env r
    typeOfFunc <- typeCheckLIdent functionCallName
    args <- traverse typeCheckExpr functionCallArgs
    typeOfFunc `shouldBeWithin` Annot (foldMap pos args) (code (unAnnot <$> args) top)
typeCheckStatement' env r Assignment{..} = do
    shouldNotBeTopLevelStatement env r
    typeOfVar <- typeCheckLIdent assignmentVariable
    exprType <- typeCheckExpr assignmentValue
    exprType `shouldBeWithin` typeOfVar
typeCheckStatement' env r IfStatement{..} = do
    shouldNotBeTopLevelStatement env r
    typeOfCond <- typeCheckExpr ifStatementCondition
    typeOfCond `shouldBeWithin` Annot (pos ifStatementCondition) (simpleType Bool)
    typeCheckStatement (notTopLevel env) ifStatementTrue
    traverse_ (typeCheckStatement (notTopLevel env)) ifStatementFalse
typeCheckStatement' env r WhileLoop{..} = do
    shouldNotBeTopLevelStatement env r
    typeOfCond <- typeCheckExpr whileLoopCondition
    typeOfCond `shouldBeWithin` Annot (pos whileLoopCondition) (simpleType Bool)
    typeCheckStatement (notTopLevel env) whileLoopStatement
typeCheckStatement' env r (Return Nothing) =
     Annot r (simpleType Nil) `shouldBeWithin` returnType env
typeCheckStatement' env _ (Return (Just x)) = do
    exprType <- typeCheckExpr x
    exprType `shouldBeWithin` returnType env

typeCheckLIdent :: Annot ResolvedLIdent -> Either TypeCheckErr (Annot Type)
typeCheckLIdent (Annot r (ResolvedLIdent initial fields)) = Annot r <$> foldrM go (identDeclToType initial) fields
    where
        go :: VarName -> Type -> Either TypeCheckErr Type
        go field x = note (NoField field (Annot r x)) $ lookupField (unVarName field) x

        identDeclToType :: ModLIdentDecl -> Type
        identDeclToType (ModFunction _ args ret) = code args ret
        identDeclToType (ModGlobalVariable _ x) = x
        identDeclToType (ModLocalVariable _ x) = x
        identDeclToType (ModCommand _ _ args ret) = code args ret
        identDeclToType (ModExternalReference _ x) = x

typeCheckExpr :: Expr Resolved -> Either TypeCheckErr (Annot Type)
typeCheckExpr (Annot r (Variable x)) = typeCheckLIdent x
typeCheckExpr (Annot r (FuncCall f args)) = do
    func <- typeCheckLIdent f
    args' <- traverse typeCheckExpr args
    left (\(s,l) -> NotWithin (Annot (pos func) s) (Annot (foldMap pos args) l)) $ right (Annot r) $ validateFuncCall (unAnnot func) (unAnnot <$> args')
typeCheckExpr (Annot r (BoolLiteral x)) = return $ Annot r (constBool x)
typeCheckExpr (Annot r (NumLiteral x)) = return $ Annot r (constNumber x)
typeCheckExpr (Annot r (StringLiteral x)) = return $ Annot r (constString x)
typeCheckExpr (Annot r (ArrayExpr xs)) = do
    types <- traverse typeCheckExpr xs
    return $ Annot r $ array $ mconcat $ unAnnot <$> types
typeCheckExpr (Annot range (BinOp o@(Annot _ op) l r)) = do
    (Annot _ l') <- typeCheckExpr l
    (Annot _ r') <- typeCheckExpr r
    Annot range <$> typeCheckBinOp l' r'
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
    >>= (left (InvalidUnOp o) . validateUnOp [(Number, Number)] . unAnnot)
typeCheckExpr (Annot range (UnOp o@(Annot _ NotOp) x)) = fmap (Annot range) $
    typeCheckExpr x
    >>= (left (InvalidUnOp o) . validateUnOp [(Bool, Bool)] . unAnnot)
typeCheckExpr (Annot _ (Cast typ x)) = do
    _ <- typeCheckExpr x
    return typ
typeCheckExpr (Annot r (Tuple xs)) = Annot r . tuple . map unAnnot <$> traverse typeCheckExpr xs
