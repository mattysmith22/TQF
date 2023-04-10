{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module TQF.CodeGen where

import           SQF.AST           (SQF, SQFType (..))
import qualified SQF.AST           as SQF
import           TQF.AST
import           TQF.AST.Annotated

codeGen :: Module Resolved -> [SQF 'SStmt]
codeGen Module{..} = concatMap codeGenDecl moduleDeclarations

newtype CodeGenEnv = CodeGenEnv
    { envFunction :: String
    }

codeGenDecl :: Declaration Resolved -> [SQF 'SStmt]
codeGenDecl decl = unAnnot $ codeGenDecl' <$> decl

codeGenDecl' :: Declaration_ Resolved -> [SQF 'SStmt]
codeGenDecl' FunctionDecl{..} = let
    functionPath = lIdentSQFName functionName
    compiledStatements = fmap (codeGenStmt env) functionContent
    paramsStatement = SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . snd) functionArguments
    env = CodeGenEnv functionPath
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:compiledStatements)]
codeGenDecl' _ = []



ensureBlock :: SQF 'SStmt -> SQF 'SExpr
ensureBlock (SQF.CodeBlock xs) = SQF.CodeBlock xs
ensureBlock x                  = SQF.CodeBlock [x]

codeGenStmt :: CodeGenEnv -> Statement Resolved -> SQF 'SStmt
codeGenStmt env stmt = unAnnot $ f <$> stmt
    where
        f (VariableDeclaration _ idnt mval) = case mval of
            Nothing     -> SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName idnt)
            (Just expr) -> SQF.Assign SQF.Private ("_" ++ unVarName idnt) (codeGenExpr env expr)
        f (Assignment idnt expr) = setLIdent idnt (codeGenExpr env expr)
        f (Expr x) = SQF.forceStmt $ codeGenExpr env x

codeGenExpr :: CodeGenEnv -> Expr Resolved -> SQF 'SExpr
codeGenExpr env x = f $ unAnnot x
    where
        f :: Expr_ Resolved -> SQF 'SExpr
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
                loopExpr = SQF.CodeBlock $ fmap (codeGenStmt env) stmt
            in SQF.BinOp "do" (SQF.UnOp "while" (SQF.CodeBlock [SQF.forceStmt $ codeGenExpr env cond])) loopExpr
        f (Variable lIdent) = getLIdent lIdent
        f (FuncCall lIdent args) = SQF.BinOp "call" (SQF.Array $ codeGenExpr env <$> args) (getLIdent lIdent)
        f (BoolLiteral x) = SQF.BoolLit x
        f (NumLiteral x) = SQF.NumLit x
        f (StringLiteral x) = SQF.StringLit x
        f (ArrayExpr xs) = SQF.Array $ codeGenExpr env <$> xs
        f (Cast _ x) = codeGenExpr env x
        f (Tuple xs) = SQF.Array $ fmap (codeGenExpr env) xs
        f (UnOp op x) = SQF.UnOp (toString $ unAnnot op) $ codeGenExpr env x
            where
                toString NegOp = "-"
                toString NotOp = "!"
        f (BinOp op l r) = SQF.BinOp (toString $ unAnnot op) (codeGenExpr env l) (codeGenExpr env r)
            where
                toString AndOp          = "&&"
                toString OrOp           = "||"
                toString AddOp          = "+"
                toString SubOp          = "-"
                toString DivOp          = "/"
                toString MulOp          = "*"
                toString ModOp          = "%"
                toString EqOp           = "isEqualTo"
                toString NotEqOp        = "isNotEqualTo"
                toString LessOp         = "<"
                toString GreaterOp      = ">"
                toString LessEqualOp    = "<="
                toString GreaterEqualOp = ">="
        f NilLit = SQF.NulOp "nil"

getLIdent :: Annot (Ident Resolved) -> SQF 'SExpr
getLIdent = getLIdent' . unAnnot

getLIdent' :: Ident Resolved -> SQF 'SExpr
getLIdent' (Ident topDecl _ fields) = foldr addFieldGet topLevelVar fields
    where
        topLevelVar = case lIdentKind topDecl of
            ValueKind         ->  SQF.Variable $ lIdentSQFName topDecl
            NularCommandKind  -> SQF.CodeBlock [SQF.NulOp $ lIdentSQFName topDecl]
            UnaryCommandKind  -> SQF.CodeBlock [SQF.UnOp (lIdentSQFName topDecl) (argNum 0)]
            BinaryCommandKind -> SQF.CodeBlock [SQF.BinOp (lIdentSQFName topDecl) (argNum 0) (argNum 1)]
        addFieldGet fieldName expr = SQF.BinOp "get" expr (SQF.StringLit $ unVarName $ unAnnot fieldName)

        argNum :: Double -> SQF 'SExpr
        argNum x = SQF.BinOp "select" (SQF.Variable "_this") (SQF.NumLit x)

setLIdent :: Annot (Ident Resolved) -> SQF 'SExpr -> SQF 'SStmt
setLIdent lident = setLIdent' (unAnnot lident)

setLIdent' :: Ident Resolved -> SQF 'SExpr -> SQF 'SStmt
setLIdent' (Ident x _ []) expr = SQF.Assign SQF.NoPrivate (lIdentSQFName x) expr
setLIdent' (Ident x args fields) expr = SQF.BinOp "set" (getLIdent' (Ident x args $ init fields)) $ SQF.Array [SQF.StringLit $ unVarName $ unAnnot $ last fields, expr]
