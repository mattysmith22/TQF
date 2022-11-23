module TQF.CodeGen.Optimiser where

import SQF.AST
import Safe (atMay)
import Data.Maybe (fromMaybe)

optimiseCommandCallStmt :: Statement -> Statement
optimiseCommandCallStmt (Assign s i e) = Assign s i $ optimiseCommandCallExpr e
optimiseCommandCallStmt (Expr e) = Expr $ optimiseCommandCallExpr e

nil :: Expr
nil = NulOp "nil"

optimiseCommandCallExpr :: Expr -> Expr
optimiseCommandCallExpr (BinOp "call" (Array args) (CodeBlock
    [Expr (BinOp cmd
        (BinOp "select" (Variable "_this") (NumLit 0))
        (BinOp "select" (Variable "_this") (NumLit 1)))]))
            = BinOp cmd (fromMaybe nil $ args' `atMay` 0) (fromMaybe nil $ args' `atMay` 1)
    where
        args' = fmap optimiseCommandCallExpr args
optimiseCommandCallExpr (BinOp "call" (Array args) (CodeBlock
    [Expr (UnOp cmd
        (BinOp "select" (Variable "_this") (NumLit 0)))]))
            = UnOp cmd (fromMaybe nil $ args' `atMay` 0)
    where
        args' = fmap optimiseCommandCallExpr args
optimiseCommandCallExpr (BinOp "call" _ (CodeBlock
    [Expr (NulOp cmd)]))
            = NulOp cmd
optimiseCommandCallExpr (BinOp op l r) = BinOp op (optimiseCommandCallExpr l) (optimiseCommandCallExpr r)
optimiseCommandCallExpr (UnOp op x) = UnOp op (optimiseCommandCallExpr x)
optimiseCommandCallExpr (Array xs) = Array $ fmap optimiseCommandCallExpr xs
optimiseCommandCallExpr (CodeBlock xs) = CodeBlock $ fmap optimiseCommandCallStmt xs
optimiseCommandCallExpr x = x