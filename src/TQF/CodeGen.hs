{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module TQF.CodeGen
    ( codeGen
    -- * Code Generation notes
    -- $codeblock-nil-suffix
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
    functionPath = lIdentSQFName $ unAnnot functionName
    paramsStatement = SQF.UnOp "params" $ SQF.Array $ fmap (SQF.StringLit . lIdentSQFName . unAnnot . snd) functionArguments
    in [SQF.Assign SQF.NoPrivate functionPath $ codeGenCodeBlock' [paramsStatement] functionContent]
codeGenDecl' _ = []

codeGenCodeBlock :: [Statement Resolved] -> SQF 'SExpr
codeGenCodeBlock = codeGenCodeBlock' []

codeGenCodeBlock' :: [SQF 'SStmt] -> [Statement Resolved] -> SQF 'SExpr
codeGenCodeBlock' prefix [] = SQF.CodeBlock prefix
codeGenCodeBlock' prefix xs = SQF.CodeBlock $ prefix ++ convertedStmts ++ nilSuffix
    where
        needsNilSuffix SQF.Assign{} = True
        needsNilSuffix _            = False

        convertedStmts = fmap codeGenStmt xs

        -- See $codeblock-nil-suffix
        nilSuffix = [SQF.NulOp "nil" | needsNilSuffix $ last convertedStmts]

codeGenStmt :: Statement Resolved -> SQF 'SStmt
codeGenStmt stmt = unAnnot $ f <$> stmt
    where
        f (VariableDeclaration _ idnt mval) = case mval of
            Nothing     -> SQF.UnOp "private" $ SQF.StringLit (lIdentSQFName $ unAnnot idnt)
            (Just expr) -> SQF.Assign SQF.Private (lIdentSQFName $ unAnnot idnt) (codeGenExpr expr)
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
                            (codeGenCodeBlock ifTrue)
                    (ThenDo ifTrue (Just ifFalse)) ->
                        SQF.BinOp "then"
                            (SQF.UnOp "if" cond')
                            (SQF.BinOp "else"
                                (codeGenCodeBlock ifTrue)
                                (codeGenCodeBlock ifFalse))
                    (ThenExitWith expr) ->
                        SQF.BinOp "exitWith"
                            (SQF.UnOp "if" cond')
                            (codeGenCodeBlock expr)
        f (WhileLoop cond stmt) =
            let
                loopExpr = codeGenCodeBlock stmt
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

-- $codeblock-nil-suffix
-- With both SQF and TQF, the value from the last statement in a code block is
-- the return value of the code block when executed.
--
-- In SQF, if you try to retrieve the value of an assignment using this method,
-- you get an error, as shown by the example SQF
--
-- > private _ret = [] call {private _x = 1;};
-- > format ["%1:%2:%3", _ret, isNil "_ret", typeName _ret];
--
-- > Error GIAS pre stack size violation
--
-- The semantics of TQF require each item to have a return type. For this reason
-- we took the step of, whenever a code block ends with an assignment, inserting
-- a nil as a suffix so that the code block will return a type.
