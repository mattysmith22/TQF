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
    compiledStatements = fmap (codeGenStmt env) functionContent
    paramsStatement = SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . snd) functionArguments
    env = CodeGenEnv functionPath
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:compiledStatements)]
codeGenDecl' _ _ = []

codeGenExpr :: CodeGenEnv -> Expr Resolved -> SQF SExpr
codeGenExpr env = SQF.forceExpr . codeGenStmt env

ensureBlock :: SQF SStmt -> SQF SExpr
ensureBlock (SQF.CodeBlock xs) = SQF.CodeBlock xs
ensureBlock x = SQF.CodeBlock [x]

codeGenStmt :: CodeGenEnv -> Expr Resolved -> SQF SStmt
codeGenStmt env stmt = unAnnot $ f <$> stmt
    where
        f (VariableDeclaration _ idnt mval) = case mval of
            Nothing -> SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName idnt)
            (Just expr) -> SQF.Assign SQF.Private ("_" ++ unVarName idnt) (codeGenExpr env expr)
        f (Assignment idnt expr) = setLIdent idnt (codeGenExpr env expr)
        f (IfStatement cond subStmts) =
            let cond' = codeGenExpr env cond
            in case subStmts of
                    (ThenDo ifTrue Nothing) ->
                        SQF.BinOp "then"
                            (SQF.UnOp "if" cond')
                            (SQF.CodeBlock (fmap (codeGenStmt env) ifTrue))
                    (ThenDo ifTrue (Just ifFalse)) -> 
                        SQF.BinOp "then"
                            (SQF.UnOp "if" cond')
                            (SQF.BinOp "else"
                                (SQF.CodeBlock (fmap (codeGenStmt env) ifTrue))
                                (SQF.CodeBlock (fmap (codeGenStmt env) ifFalse)))
                    (ThenExitWith expr) ->
                        SQF.BinOp "exitWith"
                            (SQF.UnOp "if" cond')
                            (SQF.CodeBlock (fmap (codeGenStmt env) expr))
        f (WhileLoop cond stmt) =
            let
                loopExpr = ensureBlock $ SQF.CodeBlock $ fmap (codeGenStmt env) stmt
            in SQF.BinOp "do" (SQF.UnOp "while" (ensureBlock $ codeGenStmt env cond)) loopExpr
        f (Variable lIdent) = SQF.forceStmt $ getLIdent lIdent
        f (FuncCall lIdent args) = SQF.BinOp "call" (SQF.Array $ codeGenExpr env <$> args) (getLIdent lIdent)
        f (BoolLiteral x) = SQF.BoolLit x
        f (NumLiteral x) = SQF.NumLit x
        f (StringLiteral x) = SQF.StringLit x
        f (ArrayExpr xs) = SQF.Array $ codeGenExpr env <$> xs
        f (Cast _ x) = codeGenStmt env x
        f (Tuple xs) = SQF.Array $ fmap (codeGenExpr env) xs
        f (UnOp op x) = SQF.UnOp (toString $ unAnnot op) $ codeGenExpr env x
            where
                toString NegOp = "-"
                toString NotOp = "!"
        f (BinOp op l r) = SQF.BinOp (toString $ unAnnot op) (codeGenExpr env l) (codeGenExpr env r)
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
