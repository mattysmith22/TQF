{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TQF.CodeGen.Optimiser
    ( runOptimisations
    ) where

import           Control.Arrow
import           Data.Maybe    (fromMaybe)
import           Safe          (atMay)
import           SQF.AST
import           TQF.CodeGen

nil :: SQF 'SExpr
nil = NulOp "nil"

runOptimisations :: GeneratedSQF -> GeneratedSQF
runOptimisations g = g
    { generatedFunctions = second (fmap optimiseCommandCall)
        <$> generatedFunctions g
    }

optimiseCommandCall :: SQF a -> SQF a
optimiseCommandCall (Assign s i e) = Assign s i $ optimiseCommandCall e
optimiseCommandCall (BinOp "call" (Array args) (CodeBlock
    [BinOp cmd
        (BinOp "select" (Variable "_this") (NumLit 0))
        (BinOp "select" (Variable "_this") (NumLit 1))]))
            = BinOp cmd (fromMaybe nil $ args' `atMay` 0) (fromMaybe nil $ args' `atMay` 1)
    where
        args' = fmap optimiseCommandCall args
optimiseCommandCall (BinOp "call" (Array args) (CodeBlock
    [UnOp cmd
        (BinOp "select" (Variable "_this") (NumLit 0))]))
            = UnOp cmd (fromMaybe nil $ args' `atMay` 0)
    where
        args' = fmap optimiseCommandCall args
optimiseCommandCall (BinOp "call" _ (CodeBlock
    [NulOp cmd]))
            = NulOp cmd
optimiseCommandCall (BinOp op l r) = BinOp op (optimiseCommandCall l) (optimiseCommandCall r)
optimiseCommandCall (UnOp op x) = UnOp op (optimiseCommandCall x)
optimiseCommandCall (Array xs) = Array $ fmap optimiseCommandCall xs
optimiseCommandCall (CodeBlock xs) = CodeBlock $ fmap optimiseCommandCall xs
optimiseCommandCall x = x
