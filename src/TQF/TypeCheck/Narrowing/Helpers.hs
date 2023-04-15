{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module TQF.TypeCheck.Narrowing.Helpers
    ( Inference
    -- * Matching operators
    -- ** AST matching
    , isBinaryOperator
    , isUnaryOperator
    , isFunctionCall
    , isBoolLiteral
    , isVariable
    , isNumLiteral
    , isStringLiteral
    , isArrayExpr
    , isCast
    , isTuple
    , isFieldAccess
    , isIfStatement
    , isWhileLoop
    , isNilLit
    -- ** Ident matching
    , isSQFBinaryCommandIdent
    , isSQFUnaryCommandIdent
    , isSQFNularCommandIdent
    , isSQFValueIdent
    -- ** Complex matches
    , isSQFBinaryCommandCall
    , isSQFUnaryCommandCall
    , isSQFNularCommandCall
    , isSQFFunctionCall
    ) where

import           Control.Arrow
import           Data.Functor.Foldable
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Resolve           (Resolved)
import           TQF.TypeCheck.Types

type Inference a = Maybe a

isBinaryOperator :: ExprF typeDecl ident block expr-> Inference (BinaryOperator, expr, expr)
isBinaryOperator expr
    | BinOpF op l r <- expr = return (unAnnot op, unAnnot l, unAnnot r)
    | otherwise = Nothing

isUnaryOperator :: ExprF typeDecl ident block expr-> Inference (UnaryOperator, expr)
isUnaryOperator expr
    | UnOpF op x <- expr = return (unAnnot op, unAnnot x)
    | otherwise = Nothing

isFunctionCall :: ExprF typeDecl ident block expr-> Inference (expr, [expr])
isFunctionCall expr
    | FuncCallF f args <- expr = return (unAnnot f, unAnnot <$> args)
    | otherwise = Nothing

isBoolLiteral :: ExprF typeDecl ident block expr-> Inference Bool
isBoolLiteral expr
    | BoolLiteralF x <- expr = return x
    | otherwise = Nothing

isVariable :: ExprF typeDecl ident block expr -> Inference ident
isVariable expr
    | VariableF x <- expr = return $ unAnnot x
    | otherwise = Nothing

isNumLiteral :: ExprF typeDecl ident block expr-> Inference Double
isNumLiteral expr
    | NumLiteralF x <- expr = return x
    | otherwise = Nothing

isStringLiteral :: ExprF typeDecl ident block expr-> Inference String
isStringLiteral expr
    | StringLiteralF x <- expr = return x
    | otherwise = Nothing

isArrayExpr :: ExprF typeDecl ident block expr-> Inference [expr]
isArrayExpr expr
    | ArrayExprF xs <- expr = return $ unAnnot <$> xs
    | otherwise = Nothing

isCast :: ExprF typeDecl ident block expr-> Inference (typeDecl, expr)
isCast expr
    | CastF typ x <- expr = return (unAnnot typ, unAnnot x)
    | otherwise = Nothing

isTuple :: ExprF typeDecl ident block expr-> Inference [expr]
isTuple expr
    | TupleF xs <- expr = return $ unAnnot <$> xs
    | otherwise = Nothing

isFieldAccess :: ExprF typeDecl ident block expr-> Inference (expr, Annot VarName)
isFieldAccess expr
    | FieldAccessF x field <- expr = return (unAnnot x,field)
    | otherwise = Nothing

isIfStatement :: ExprF typeDecl ident block expr-> Inference (expr, IfTrue block)
isIfStatement expr
    | IfStatementF cond blocks <- expr = return (unAnnot cond, blocks)
    | otherwise = Nothing

isWhileLoop :: ExprF typeDecl ident block expr-> Inference (expr, block)
isWhileLoop expr
    | WhileLoopF cond block <- expr = return (unAnnot cond, block)
    | otherwise = Nothing

isNilLit :: ExprF typeDecl ident block expr-> Inference ()
isNilLit expr
    | NilLitF <- expr = return ()
    | otherwise = Nothing

isSQFBinaryCommandIdent :: Ident Resolved -> Inference String
isSQFBinaryCommandIdent ident
    | ModLIdentDecl{..} <- identName ident
    , BinaryCommandKind <- lIdentKind = return lIdentSQFName
    | otherwise = Nothing

isSQFUnaryCommandIdent :: Ident Resolved -> Inference String
isSQFUnaryCommandIdent ident
    | ModLIdentDecl{..} <- identName ident
    , UnaryCommandKind <- lIdentKind = return lIdentSQFName
    | otherwise = Nothing

isSQFNularCommandIdent :: Ident Resolved -> Inference String
isSQFNularCommandIdent ident
    | ModLIdentDecl{..} <- identName ident
    , NularCommandKind <- lIdentKind = return lIdentSQFName
    | otherwise = Nothing

isSQFValueIdent :: Ident Resolved -> Inference String
isSQFValueIdent ident
    | ModLIdentDecl{..} <- identName ident
    , ValueKind <- lIdentKind = return lIdentSQFName
    | otherwise = Nothing

isSQFBinaryCommandCall
    :: (Base expr ~ ExprF typeDecl' (Ident Resolved) block', Recursive expr)
    => ExprF typeDecl (Ident Resolved, MGType) block (expr,b)
    -> Inference (String, (expr,b), (expr,b))
isSQFBinaryCommandCall expr = do
    (f,args) <- isFunctionCall expr
    sqf <- isSQFBinaryCommandIdent =<< isVariable (project $ fst f)
    case args of
        (l:r:_) -> return (sqf, l, r)
        _       -> Nothing

isSQFUnaryCommandCall
    :: (Base expr ~ ExprF typeDecl' (Ident Resolved, MGType) block', Recursive expr)
    => ExprF typeDecl (Ident Resolved, MGType) block (expr,b)
    -> Inference (String, (Base expr expr,b))
isSQFUnaryCommandCall expr = do
    (f,args) <- isFunctionCall expr
    sqf <- isSQFUnaryCommandIdent . fst =<< isVariable (project $ fst f)
    case args of
        (x:_) -> return (sqf, first project x)
        _     -> Nothing

isSQFNularCommandCall
    :: (Base expr ~ ExprF typeDecl (Ident Resolved, MGType) block, Recursive expr)
    => ExprF typeDecl (Ident Resolved, MGType) block (expr,b) -> Inference String
isSQFNularCommandCall expr
    = isSQFNularCommandIdent . fst  =<< isVariable . project . fst . fst =<< isFunctionCall expr

isSQFFunctionCall
    :: (Base expr ~ ExprF typeDecl (Ident Resolved, MGType) block, Recursive expr)
    => ExprF typeDecl (Ident Resolved, MGType) block (expr,b)
    -> Inference (String, [(Base expr expr,b)])
isSQFFunctionCall expr = do
    (f,args) <- isFunctionCall expr
    sqf <- isSQFValueIdent . fst =<< isVariable (project $ fst f)
    return (sqf, first project <$> args)
