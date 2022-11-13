{-# LANGUAGE RecordWildCards #-}
module TQF.TypeCheck
    ( TypeCheckErr(..)
    , typeCheck
    ) where

import TQF.AST
import TQF.Type
import Control.Error.Util (note)
import Control.Monad (when)
import Control.Arrow
import Data.Foldable (foldrM, traverse_)

data TypeCheckErr = NotWithin Type Type
     | NoField VarName Type
     | BadTopLevel
     | NoValidCommand [Type]
     deriving (Show, Eq)

shouldBeWithin :: Type -> Type -> Either TypeCheckErr ()
shouldBeWithin small large = if small `isWithin` large then return () else Left $ NotWithin small large

typeCheck :: Module Resolved -> Either TypeCheckErr ()
typeCheck Module{..} = mapM_ typeCheckDeclaration moduleDeclarations

typeCheckDeclaration :: Declaration Resolved -> Either TypeCheckErr ()
typeCheckDeclaration  FunctionDecl{..} = typeCheckStatement (Env functionType True) functionContent
typeCheckDeclaration VariableDecl{..} = return ()
typeCheckDeclaration TypeDecl{..} = return ()
typeCheckDeclaration CommandDecl{..} = return ()
typeCheckDeclaration ExternalFunctionDecl{..} = return ()
typeCheckDeclaration ExternalVariableDecl{..} = return ()

data Env = Env
    { returnType :: Type
    , topLevelStatement :: Bool
    }

shouldNotBeTopLevelStatement :: Env -> Either TypeCheckErr ()
shouldNotBeTopLevelStatement Env{..} = when topLevelStatement (Left BadTopLevel)

notTopLevel :: Env -> Env
notTopLevel e = e {topLevelStatement = False}

typeCheckStatement :: Env -> Statement Resolved -> Either TypeCheckErr ()
typeCheckStatement env (CodeBlock stmts) = traverse_ (typeCheckStatement (notTopLevel env)) stmts
typeCheckStatement env VariableDeclaration{..} = do
    shouldNotBeTopLevelStatement env
    exprType <- maybe (return $ simpleType Nil) typeCheckExpr varDeclValue
    exprType `shouldBeWithin` varDeclType
typeCheckStatement env FunctionCall{..} = do
    shouldNotBeTopLevelStatement env
    typeOfFunc <- typeCheckLIdent functionCallName
    args <- traverse typeCheckExpr functionCallArgs
    typeOfFunc `shouldBeWithin` code args top
typeCheckStatement env Assignment{..} = do
    shouldNotBeTopLevelStatement env
    typeOfVar <- typeCheckLIdent assignmentVariable
    exprType <- typeCheckExpr assignmentValue
    exprType `shouldBeWithin` typeOfVar
typeCheckStatement env IfStatement{..} = do
    shouldNotBeTopLevelStatement env
    typeOfCond <- typeCheckExpr ifStatementCondition
    typeOfCond `shouldBeWithin` simpleType Bool
    typeCheckStatement (notTopLevel env) ifStatementTrue
    traverse_ (typeCheckStatement (notTopLevel env)) ifStatementFalse
typeCheckStatement env WhileLoop{..} = do
    shouldNotBeTopLevelStatement env
    typeOfCond <- typeCheckExpr whileLoopCondition
    typeOfCond `shouldBeWithin` simpleType Bool
    typeCheckStatement (notTopLevel env) whileLoopStatement
typeCheckStatement env (Return Nothing) =
    simpleType Nil `shouldBeWithin` returnType env
typeCheckStatement env (Return (Just x)) = do
    exprType <- typeCheckExpr x
    exprType `shouldBeWithin` returnType env
typeCheckStatement env (DirectCallStmt (_,cmds) exprs) = do
    types <- traverse typeCheckExpr exprs
    left (const $ NoValidCommand types) $ validateDirectCall cmds types
    return ()

typeCheckLIdent :: ResolvedLIdent -> Either TypeCheckErr Type
typeCheckLIdent (ResolvedLIdent initial fields) = foldrM go (identDeclToType initial) fields
    where
        go :: VarName -> Type -> Either TypeCheckErr Type
        go field x = note (NoField field x) $ lookupField (unVarName field) x

        identDeclToType :: ModLIdentDecl -> Type
        identDeclToType (ModFunction _ args ret) = code args ret
        identDeclToType (ModGlobalVariable _ x) = x
        identDeclToType (ModLocalVariable _ x) = x
        identDeclToType (ModExternalReference _ x) = x

typeCheckExpr :: Expr Resolved -> Either TypeCheckErr Type
typeCheckExpr (Variable x) = typeCheckLIdent x
typeCheckExpr (FuncCall f args) = do
    func <- typeCheckLIdent f
    args' <- traverse typeCheckExpr args
    left (uncurry NotWithin) $ validateFuncCall func args'
typeCheckExpr (BoolLiteral x) = Right (constBool x)
typeCheckExpr (NumLiteral x) = Right (constNumber x)
typeCheckExpr (StringLiteral x) = Right (constString x)
typeCheckExpr (ArrayExpr xs) = do
    types <- traverse typeCheckExpr xs
    return $ array $ mconcat types
typeCheckExpr (DirectCall (_,cmds) exprs) = do
    types <- traverse typeCheckExpr exprs
    left (const $ NoValidCommand types) $ validateDirectCall cmds types
typeCheckExpr (Cast typ x) = do
    _ <- typeCheckExpr x
    return typ