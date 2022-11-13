{-# LANGUAGE RecordWildCards #-}
module TQF.CodeGen where

import TQF.AST
import qualified SQF.AST as SQF
import           Data.List (intercalate)

codeGen :: Module Resolved -> [SQF.Statement]
codeGen Module{..} = concatMap (codeGenDecl moduleName) moduleDeclarations

newtype CodeGenEnv = CodeGenEnv
    { envFunction :: String
    }

codeGenDecl :: ResolveableModule -> Declaration Resolved -> [SQF.Statement]
codeGenDecl modPath FunctionDecl{..} = let
    functionPath = sqfNameFor (ModFunction (modPath, functionName) [] mempty)
    compiledStatements = codeGenTopLevelStatement env functionContent
    paramsStatement = SQF.Expr $ SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . ("_"++) . unVarName . snd) functionArguments
    scopeName = SQF.Expr $ SQF.UnOp "scopeName" $ SQF.StringLit functionPath
    env = CodeGenEnv functionPath
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:scopeName:compiledStatements)]
codeGenDecl _ _ = []

codeGenTopLevelStatement :: CodeGenEnv -> Statement Resolved -> [SQF.Statement]
codeGenTopLevelStatement env (CodeBlock xs) = fmap (codeGenStatement env) xs
codeGenTopLevelStatement env x = [codeGenStatement env x]

codeGenStatement :: CodeGenEnv -> Statement Resolved -> SQF.Statement
codeGenStatement env = either id SQF.Expr . codeGenStatement' env

codeGenStatementExpr :: CodeGenEnv -> Statement Resolved -> SQF.Expr
codeGenStatementExpr env = either (\x -> SQF.UnOp "call" $ SQF.CodeBlock [x]) id . codeGenStatement' env

codeGenStatement' :: CodeGenEnv -> Statement Resolved -> Either SQF.Statement SQF.Expr
codeGenStatement' env (CodeBlock xs) = Right $ SQF.CodeBlock $ fmap (codeGenStatement env) xs
codeGenStatement' env VariableDeclaration{..} = case varDeclValue of
    Nothing -> Right $ SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName varDeclName)
    (Just expr) -> Left $ SQF.Assign SQF.Private ("_" ++ unVarName varDeclName) (codeGenExpr expr)
codeGenStatement' env FunctionCall{..} = let
    args = codeGenExpr <$> functionCallArgs
    lident = getLIdent functionCallName
    in Right $ SQF.BinOp "call" (SQF.Array args) lident
codeGenStatement' env Assignment{..} = setLIdent assignmentVariable (codeGenExpr assignmentValue)
codeGenStatement' env IfStatement{..} =
    let cond = codeGenExpr ifStatementCondition
        trueExpr = codeGenStatementExpr env ifStatementTrue
        mFalseExpr = codeGenStatementExpr env <$> ifStatementFalse
        falseWrapper = maybe id (flip $ SQF.BinOp "else") mFalseExpr
    in Right $ falseWrapper $ SQF.BinOp "then" (SQF.UnOp "if" cond) trueExpr
codeGenStatement' env WhileLoop{..} =
    let cond = codeGenExpr whileLoopCondition 
        loopExpr = codeGenStatementExpr env whileLoopStatement
    in Right $ SQF.BinOp "do" (SQF.UnOp "while" (SQF.CodeBlock [SQF.Expr cond])) loopExpr
codeGenStatement' env (Return Nothing) =
    Right $ SQF.UnOp "breakOut" $ SQF.StringLit $ envFunction env
codeGenStatement' env (Return (Just expr)) =
    let retVal = codeGenExpr expr
    in Right $ SQF.BinOp "breakOut" retVal $ SQF.StringLit $ envFunction env
codeGenStatement' env (DirectCallStmt cmd args) = Right $ codeGenExpr (DirectCall cmd args)

codeGenExpr :: Expr Resolved -> SQF.Expr
codeGenExpr (Variable lIdent) = getLIdent lIdent
codeGenExpr (FuncCall lIdent args) = SQF.BinOp "call" (SQF.Array $ codeGenExpr <$> args) (getLIdent lIdent)
codeGenExpr (BoolLiteral x) = SQF.BoolLit x
codeGenExpr (NumLiteral x) = SQF.NumLit x
codeGenExpr (StringLiteral x) = SQF.StringLit x
codeGenExpr (ArrayExpr xs) = SQF.Array $ codeGenExpr <$> xs
codeGenExpr (DirectCall (name,_) []) = SQF.NulOp name
codeGenExpr (DirectCall (name,_) [x]) = SQF.UnOp name (codeGenExpr x)
codeGenExpr (DirectCall (name,_) [x, y]) = SQF.BinOp name (codeGenExpr x) (codeGenExpr y)
codeGenExpr (DirectCall _ xs) = error $ "Cannot CodeGen a direct call with " ++ show xs ++ " arguments"
codeGenExpr (Cast _ x) = codeGenExpr x

getLIdent :: ResolvedLIdent -> SQF.Expr
getLIdent (ResolvedLIdent x fields) = foldr (\field acc -> SQF.BinOp "get" acc (SQF.StringLit $ unVarName field)) (SQF.Variable $ sqfNameFor x) fields

setLIdent :: ResolvedLIdent -> SQF.Expr -> Either SQF.Statement SQF.Expr
setLIdent (ResolvedLIdent x []) expr = Left $ SQF.Assign SQF.NoPrivate (sqfNameFor x) expr
setLIdent (ResolvedLIdent x fields) expr = Right $ SQF.BinOp "set" (getLIdent (ResolvedLIdent x (init fields))) $ SQF.Array [SQF.StringLit $ unVarName $ last fields, expr]

sqfNameFor :: ModLIdentDecl -> String
sqfNameFor (ModFunction (path, name) _ _)
    = intercalate "_" (fmap unTypeName path) ++ "_fnc_"  ++ unVarName name
sqfNameFor (ModGlobalVariable (path, name) _)
    = intercalate "_" (fmap unTypeName path ++ [unVarName name])
sqfNameFor (ModLocalVariable name _)
    = "_" ++ unVarName name
sqfNameFor (ModExternalReference name _) = name