{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module TQF.CodeGen
    ( codeGen
    ) where

import           SQF.AST           (SQF, SQFType (..))
import qualified SQF.AST           as SQF
import           TQF.AST
import           TQF.AST.Annotated

codeGen :: Module Resolved -> [SQF 'SStmt]
codeGen Module{..} = concatMap codeGenDecl moduleDeclarations

codeGenDecl :: Declaration Resolved -> [SQF 'SStmt]
codeGenDecl decl = unAnnot $ codeGenDecl' <$> decl

codeGenDecl' :: Declaration_ Resolved -> [SQF 'SStmt]
codeGenDecl' FunctionDecl{..} = let
    functionPath = lIdentSQFName functionName
    compiledStatements = fmap codeGenStmt functionContent
    paramsStatement = SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . snd) functionArguments
    in [SQF.Assign SQF.NoPrivate functionPath $ SQF.CodeBlock (paramsStatement:compiledStatements)]
codeGenDecl' _ = []

codeGenStmt :: Statement Resolved -> SQF 'SStmt
codeGenStmt stmt = unAnnot $ f <$> stmt
    where
        f (VariableDeclaration _ idnt mval) = case mval of
            Nothing     -> SQF.UnOp "private" $ SQF.StringLit ("_" ++ unVarName idnt)
            (Just expr) -> SQF.Assign SQF.Private ("_" ++ unVarName idnt) (codeGenExpr expr)
        f (Assignment (LValueVar ident) expr)
            = SQF.Assign SQF.NoPrivate (lIdentSQFName $ identName $ unAnnot ident) $ codeGenExpr expr
        f (Assignment (LValueField lexpr field) rexpr)
            = SQF.BinOp "set" (codeGenExpr lexpr) $
                SQF.Array
                    [ SQF.StringLit $ unVarName $ unAnnot field
                    , codeGenExpr rexpr
                    ]
        f (Expr x) = SQF.forceStmt $ codeGenExpr x

codeGenExpr :: Expr Resolved -> SQF 'SExpr
codeGenExpr x = f $ unAnnot x
    where
        f :: Expr_ Resolved -> SQF 'SExpr
        f (IfStatement cond subStmts) =
            let cond' = codeGenExpr cond
            in case subStmts of
                    (ThenDo ifTrue Nothing) ->
                        SQF.BinOp "then"
                            (SQF.UnOp "if" cond')
                            (SQF.CodeBlock (fmap codeGenStmt ifTrue))
                    (ThenDo ifTrue (Just ifFalse)) ->
                        SQF.BinOp "then"
                            (SQF.UnOp "if" cond')
                            (SQF.BinOp "else"
                                (SQF.CodeBlock (fmap codeGenStmt ifTrue))
                                (SQF.CodeBlock (fmap codeGenStmt ifFalse)))
                    (ThenExitWith expr) ->
                        SQF.BinOp "exitWith"
                            (SQF.UnOp "if" cond')
                            (SQF.CodeBlock (fmap codeGenStmt expr))
        f (WhileLoop cond stmt) =
            let
                loopExpr = SQF.CodeBlock $ fmap codeGenStmt stmt
            in SQF.BinOp "do" (SQF.UnOp "while" (SQF.CodeBlock [SQF.forceStmt $ codeGenExpr cond])) loopExpr
        f (Variable lIdent) = getIdent $ unAnnot lIdent
        f (FuncCall lIdent args) = SQF.BinOp "call" (SQF.Array $ codeGenExpr <$> args) (getIdent $ unAnnot lIdent)
        f (BoolLiteral x) = SQF.BoolLit x
        f (NumLiteral x) = SQF.NumLit x
        f (StringLiteral x) = SQF.StringLit x
        f (ArrayExpr xs) = SQF.Array $ codeGenExpr <$> xs
        f (Cast _ x) = codeGenExpr x
        f (FieldAccess x field) = SQF.BinOp "get" (codeGenExpr x) (SQF.StringLit $ unVarName $ unAnnot field)
        f (Tuple xs) = SQF.Array $ fmap codeGenExpr xs
        f (UnOp op x) = SQF.UnOp (toString $ unAnnot op) $ codeGenExpr x
            where
                toString NegOp = "-"
                toString NotOp = "!"
        f (BinOp op l r) = SQF.BinOp (toString $ unAnnot op) (codeGenExpr l) (codeGenExpr r)
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

getIdent :: Ident Resolved -> SQF 'SExpr
getIdent (Ident topDecl _) = case lIdentKind topDecl of
    ValueKind         ->  SQF.Variable $ lIdentSQFName topDecl
    NularCommandKind  -> SQF.CodeBlock [SQF.NulOp $ lIdentSQFName topDecl]
    UnaryCommandKind  -> SQF.CodeBlock [SQF.UnOp (lIdentSQFName topDecl) (argNum 0)]
    BinaryCommandKind -> SQF.CodeBlock [SQF.BinOp (lIdentSQFName topDecl) (argNum 0) (argNum 1)]
    where
        argNum :: Double -> SQF 'SExpr
        argNum x = SQF.BinOp "select" (SQF.Variable "_this") (SQF.NumLit x)
