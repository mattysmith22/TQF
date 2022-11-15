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

data TypeCheckErr = NotWithin (Annot Type) (Annot Type)
     | NoField VarName (Annot Type)
     | BadTopLevel
     | NoValidCommand String [Annot Type]
     deriving (Show, Eq)

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

shouldNotBeTopLevelStatement :: Env -> Either TypeCheckErr ()
shouldNotBeTopLevelStatement Env{..} = when topLevelStatement (Left BadTopLevel)

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
typeCheckStatement' env _ VariableDeclaration{..} = do
    shouldNotBeTopLevelStatement env
    exprType <- maybe (return $ Annot (pos varDeclType) $ simpleType Nil) typeCheckExpr varDeclValue
    exprType `shouldBeWithin` varDeclType
typeCheckStatement' env _ FunctionCall{..} = do
    shouldNotBeTopLevelStatement env
    typeOfFunc <- typeCheckLIdent functionCallName
    args <- traverse typeCheckExpr functionCallArgs
    typeOfFunc `shouldBeWithin` Annot (foldMap pos args) (code (unAnnot <$> args) top)
typeCheckStatement' env _ Assignment{..} = do
    shouldNotBeTopLevelStatement env
    typeOfVar <- typeCheckLIdent assignmentVariable
    exprType <- typeCheckExpr assignmentValue
    exprType `shouldBeWithin` typeOfVar
typeCheckStatement' env _ IfStatement{..} = do
    shouldNotBeTopLevelStatement env
    typeOfCond <- typeCheckExpr ifStatementCondition
    typeOfCond `shouldBeWithin` Annot (pos ifStatementCondition) (simpleType Bool)
    typeCheckStatement (notTopLevel env) ifStatementTrue
    traverse_ (typeCheckStatement (notTopLevel env)) ifStatementFalse
typeCheckStatement' env _ WhileLoop{..} = do
    shouldNotBeTopLevelStatement env
    typeOfCond <- typeCheckExpr whileLoopCondition
    typeOfCond `shouldBeWithin` Annot (pos whileLoopCondition) (simpleType Bool)
    typeCheckStatement (notTopLevel env) whileLoopStatement
typeCheckStatement' env r (Return Nothing) =
     Annot r (simpleType Nil) `shouldBeWithin` returnType env
typeCheckStatement' env _ (Return (Just x)) = do
    exprType <- typeCheckExpr x
    exprType `shouldBeWithin` returnType env
typeCheckStatement' env _ (DirectCallStmt (name,cmds) exprs) = do
    types <- traverse typeCheckExpr exprs
    left (const $ NoValidCommand name types) $ validateDirectCall cmds (unAnnot <$> types)
    return ()

typeCheckLIdent :: Annot ResolvedLIdent -> Either TypeCheckErr (Annot Type)
typeCheckLIdent (Annot r (ResolvedLIdent initial fields)) = Annot r <$> foldrM go (identDeclToType initial) fields
    where
        go :: VarName -> Type -> Either TypeCheckErr Type
        go field x = note (NoField field (Annot r x)) $ lookupField (unVarName field) x

        identDeclToType :: ModLIdentDecl -> Type
        identDeclToType (ModFunction _ args ret) = code args ret
        identDeclToType (ModGlobalVariable _ x) = x
        identDeclToType (ModLocalVariable _ x) = x
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
typeCheckExpr (Annot r (DirectCall (name,cmds) exprs)) = do
    types <- traverse typeCheckExpr exprs
    left (const $ NoValidCommand name types) $ right (Annot r) $ validateDirectCall cmds (unAnnot <$> types)
typeCheckExpr (Annot _ (Cast typ x)) = do
    _ <- typeCheckExpr x
    return typ