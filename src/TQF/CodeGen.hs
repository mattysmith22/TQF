{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module TQF.CodeGen where

import TQF.AST
import TQF.AST.Annotated
import Safe
import qualified SQF.AST as SQF
import SQF.AST (SQF, SQFType(..))
import           Data.List (intercalate)

codeGen :: Module Resolved -> [SQF SStmt]
codeGen Module{..} = concatMap (codeGenDecl moduleName) moduleDeclarations

newtype CodeGenEnv = CodeGenEnv
    { envFunction :: String
    }

codeGenDecl :: ResolveableModule -> Declaration Resolved -> [SQF SStmt]
codeGenDecl modPath decl = unAnnot $ codeGenDecl' modPath <$> decl

codeGenDecl' :: ResolveableModule -> Declaration_ Resolved -> [SQF SStmt]
codeGenDecl' modPath FunctionDecl{..} = let
    functionPath = lIdentSQFName functionName
    compiledStatements = codeGenTopLevelStatement env functionContent
    paramsStatement = SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . snd) functionArguments
    scopeName = SQF.UnOp "scopeName" $ SQF.StringLit functionPath
    env = CodeGenEnv functionPath
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:scopeName:compiledStatements)]
codeGenDecl' _ _ = []

codeGenTopLevelStatement :: CodeGenEnv -> Statement Resolved -> [SQF SStmt]
codeGenTopLevelStatement env stmt = unAnnot $ f <$> stmt
    where
        f (CodeBlock xs) = let 
            endStmts = case lastMay xs of
                (Just (Annot _ (Return _))) -> []
                _ -> [SQF.NulOp "nil"]
            stmts = fmap (codeGenStatement env) xs
            in stmts ++ endStmts
        f _ = [codeGenStatement env stmt]

codeGenStatementExpr :: CodeGenEnv -> Statement Resolved -> SQF SExpr
codeGenStatementExpr env = SQF.forceExpr . codeGenStatement env

codeGenStatement :: CodeGenEnv -> Statement Resolved -> SQF SStmt
codeGenStatement env stmt = unAnnot $ f <$> stmt
    where
        f (CodeBlock xs) = SQF.CodeBlock $ fmap (codeGenStatement env) xs
        f VariableDeclaration{..} = case varDeclValue of
            Nothing -> SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName varDeclName)
            (Just expr) -> SQF.Assign SQF.Private ("_" ++ unVarName varDeclName) (codeGenExpr expr)
        f FunctionCall{..} = SQF.forceStmt $ codeGenExpr' (FuncCall functionCallName functionCallArgs)
        f Assignment{..} = setLIdent assignmentVariable (codeGenExpr assignmentValue)
        f IfStatement{..} =
            let cond = codeGenExpr ifStatementCondition
                trueExpr = codeGenStatementExpr env ifStatementTrue
                mFalseExpr = codeGenStatementExpr env <$> ifStatementFalse
                falseWrapper = maybe id (flip $ SQF.BinOp "else") mFalseExpr
            in SQF.forceStmt $ falseWrapper $ SQF.BinOp "then" (SQF.UnOp "if" cond) trueExpr
        f WhileLoop{..} =
            let cond = codeGenExpr whileLoopCondition 
                loopExpr = codeGenStatementExpr env whileLoopStatement
            in SQF.BinOp "do" (SQF.UnOp "while" (SQF.CodeBlock [SQF.forceStmt cond])) loopExpr
        f (Return Nothing) =
            SQF.UnOp "breakOut" $ SQF.StringLit $ envFunction env
        f (Return (Just expr)) =
            let retVal = codeGenExpr expr
            in SQF.BinOp "breakOut" retVal $ SQF.StringLit $ envFunction env

codeGenExpr :: Expr Resolved -> SQF SExpr
codeGenExpr x = codeGenExpr' $ unAnnot x

codeGenExpr' :: Expr_ Resolved -> SQF SExpr
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

getLIdent :: Annot ResolvedLIdent -> SQF SExpr
getLIdent = getLIdent' . unAnnot

getLIdent' :: ResolvedLIdent -> SQF SExpr
getLIdent' (ResolvedLIdent topDecl fields) = foldr addFieldGet topLevelVar fields
    where
        topLevelVar = case lIdentKind topDecl of
            ValueKind ->  SQF.Variable $ lIdentSQFName topDecl
            NularCommandKind -> SQF.CodeBlock [SQF.NulOp $ lIdentSQFName topDecl]
            UnaryCommandKind -> SQF.CodeBlock [SQF.UnOp (lIdentSQFName topDecl) (argNum 0)]
            BinaryCommandKind -> SQF.CodeBlock [SQF.BinOp (lIdentSQFName topDecl) (argNum 0) (argNum 1)]
        addFieldGet fieldName expr = SQF.BinOp "get" expr (SQF.StringLit $ unVarName fieldName)

        argNum :: Double -> SQF SExpr
        argNum x = SQF.BinOp "select" (SQF.Variable "_this") (SQF.NumLit x)

setLIdent :: Annot ResolvedLIdent -> SQF SExpr -> SQF SStmt
setLIdent lident = setLIdent' (unAnnot lident)

setLIdent' :: ResolvedLIdent -> SQF SExpr -> SQF SStmt
setLIdent' (ResolvedLIdent x []) expr = SQF.Assign SQF.NoPrivate (lIdentSQFName x) expr
setLIdent' (ResolvedLIdent x fields) expr = SQF.BinOp "set" (getLIdent' (ResolvedLIdent x (init fields))) $ SQF.Array [SQF.StringLit $ unVarName $ last fields, expr]
