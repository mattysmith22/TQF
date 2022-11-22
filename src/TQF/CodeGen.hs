{-# LANGUAGE RecordWildCards #-}
module TQF.CodeGen where

import TQF.AST
import TQF.AST.Annotated
import Safe
import qualified SQF.AST as SQF
import           Data.List (intercalate)

codeGen :: Module Resolved -> [SQF.Statement]
codeGen Module{..} = concatMap (codeGenDecl moduleName) moduleDeclarations

newtype CodeGenEnv = CodeGenEnv
    { envFunction :: String
    }

codeGenDecl :: ResolveableModule -> Declaration Resolved -> [SQF.Statement]
codeGenDecl modPath decl = unAnnot $ codeGenDecl' modPath <$> decl

codeGenDecl' :: ResolveableModule -> Declaration_ Resolved -> [SQF.Statement]
codeGenDecl' modPath FunctionDecl{..} = let
    functionPath = lIdentSQFName functionName
    compiledStatements = codeGenTopLevelStatement env functionContent
    paramsStatement = SQF.Expr $ SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . snd) functionArguments
    scopeName = SQF.Expr $ SQF.UnOp "scopeName" $ SQF.StringLit functionPath
    env = CodeGenEnv functionPath
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:scopeName:compiledStatements)]
codeGenDecl' _ _ = []

codeGenTopLevelStatement :: CodeGenEnv -> Statement Resolved -> [SQF.Statement]
codeGenTopLevelStatement env stmt = unAnnot $ f <$> stmt
    where
        f (CodeBlock xs) = let 
            endStmts = case lastMay xs of
                (Just (Annot _ (Return _))) -> []
                _ -> [SQF.Expr $ SQF.NulOp "nil"]
            stmts = fmap (codeGenStatement env) xs
            in stmts ++ endStmts
        f _ = [codeGenStatement env stmt]

codeGenStatement :: CodeGenEnv -> Statement Resolved -> SQF.Statement
codeGenStatement env = either id SQF.Expr . codeGenStatement' env

codeGenStatementExpr :: CodeGenEnv -> Statement Resolved -> SQF.Expr
codeGenStatementExpr env = either (\x -> SQF.UnOp "call" $ SQF.CodeBlock [x]) id . codeGenStatement' env

codeGenStatement' :: CodeGenEnv -> Statement Resolved -> Either SQF.Statement SQF.Expr
codeGenStatement' env stmt = unAnnot $ f <$> stmt
    where
        f (CodeBlock xs) = Right $ SQF.CodeBlock $ fmap (codeGenStatement env) xs
        f VariableDeclaration{..} = case varDeclValue of
            Nothing -> Right $ SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName varDeclName)
            (Just expr) -> Left $ SQF.Assign SQF.Private ("_" ++ unVarName varDeclName) (codeGenExpr expr)
        f FunctionCall{..} = Right $ codeGenExpr' (FuncCall functionCallName functionCallArgs)
        f Assignment{..} = setLIdent assignmentVariable (codeGenExpr assignmentValue)
        f IfStatement{..} =
            let cond = codeGenExpr ifStatementCondition
                trueExpr = codeGenStatementExpr env ifStatementTrue
                mFalseExpr = codeGenStatementExpr env <$> ifStatementFalse
                falseWrapper = maybe id (flip $ SQF.BinOp "else") mFalseExpr
            in Right $ falseWrapper $ SQF.BinOp "then" (SQF.UnOp "if" cond) trueExpr
        f WhileLoop{..} =
            let cond = codeGenExpr whileLoopCondition 
                loopExpr = codeGenStatementExpr env whileLoopStatement
            in Right $ SQF.BinOp "do" (SQF.UnOp "while" (SQF.CodeBlock [SQF.Expr cond])) loopExpr
        f (Return Nothing) =
            Right $ SQF.UnOp "breakOut" $ SQF.StringLit $ envFunction env
        f (Return (Just expr)) =
            let retVal = codeGenExpr expr
            in Right $ SQF.BinOp "breakOut" retVal $ SQF.StringLit $ envFunction env

codeGenExpr :: Expr Resolved -> SQF.Expr
codeGenExpr x = codeGenExpr' $ unAnnot x

codeGenExpr' :: Expr_ Resolved -> SQF.Expr
codeGenExpr' (Variable lIdent) = getLIdent lIdent
codeGenExpr' (FuncCall lIdent args) = SQF.BinOp "call" (SQF.Array $ codeGenExpr <$> args) (getLIdent lIdent)
codeGenExpr' (BoolLiteral x) = SQF.BoolLit x
codeGenExpr' (NumLiteral x) = SQF.NumLit x
codeGenExpr' (StringLiteral x) = SQF.StringLit x
codeGenExpr' (ArrayExpr xs) = SQF.Array $ codeGenExpr <$> xs
codeGenExpr' (Cast _ x) = codeGenExpr x
codeGenExpr' (Tuple xs) = SQF.Array $ fmap codeGenExpr xs
codeGenExpr' (UnOp op x) = SQF.UnOp (toString $ unAnnot op) $ codeGenExpr x
    where
        toString NegOp = "-"
        toString NotOp = "!"
codeGenExpr' (BinOp op l r) = SQF.BinOp (toString $ unAnnot op) (codeGenExpr l) (codeGenExpr r)
    where
        toString AndOp = "&&"
        toString OrOp = "||"
        toString AddOp = "+"
        toString SubOp = "-"
        toString DivOp = "/"
        toString MulOp = "*"
        toString ModOp = "%"
        toString EqOp = "isEqualTo"
        toString NotEqOp = "isNotEqualTo"
        toString LessOp = "<"
        toString GreaterOp = ">"
        toString LessEqualOp = "<="
        toString GreaterEqualOp = ">="

getLIdent :: Annot ResolvedLIdent -> SQF.Expr
getLIdent = getLIdent' . unAnnot

getLIdent' :: ResolvedLIdent -> SQF.Expr
getLIdent' (ResolvedLIdent topDecl fields) = foldr addFieldGet topLevelVar fields
    where
        topLevelVar = case lIdentKind topDecl of
            ValueKind ->  SQF.Variable $ lIdentSQFName topDecl
            NularCommandKind -> SQF.CodeBlock [SQF.Expr $ SQF.NulOp $ lIdentSQFName topDecl]
            UnaryCommandKind -> SQF.CodeBlock [SQF.Expr $ SQF.UnOp (lIdentSQFName topDecl) (argNum 0)]
            BinaryCommandKind -> SQF.CodeBlock [SQF.Expr $ SQF.BinOp (lIdentSQFName topDecl) (argNum 0) (argNum 1)]
        addFieldGet fieldName expr = SQF.BinOp "get" expr (SQF.StringLit $ unVarName fieldName)

        argNum :: Double -> SQF.Expr
        argNum x = SQF.BinOp "select" (SQF.Variable "_this") (SQF.NumLit x)

setLIdent :: Annot ResolvedLIdent -> SQF.Expr -> Either SQF.Statement SQF.Expr
setLIdent lident = setLIdent' (unAnnot lident)

setLIdent' :: ResolvedLIdent -> SQF.Expr -> Either SQF.Statement SQF.Expr
setLIdent' (ResolvedLIdent x []) expr = Left $ SQF.Assign SQF.NoPrivate (lIdentSQFName x) expr
setLIdent' (ResolvedLIdent x fields) expr = Right $ SQF.BinOp "set" (getLIdent' (ResolvedLIdent x (init fields))) $ SQF.Array [SQF.StringLit $ unVarName $ last fields, expr]
