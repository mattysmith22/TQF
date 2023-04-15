{-# LANGUAGE TupleSections #-}
module TQF.TypeCheck.Narrowing
    ( NarrowableExpr
    , NarrowingRule
    , narrowingRules
    ) where

import           Control.Monad
import           Data.Functor.Foldable
import           TQF.AST
import           TQF.Resolve                     (Resolved)
import           TQF.Type                        (Type)
import qualified TQF.Type                        as Type
import           TQF.TypeCheck.Facts
import           TQF.TypeCheck.Monad
import           TQF.TypeCheck.Narrowing.Helpers
import           TQF.TypeCheck.Types

type NarrowableExpr = ExprF (TypeDeclF Resolved) (Ident Resolved, MGType) ([Statement Resolved], MGType) (Expr_ Resolved, MGType)

type NarrowingRule
    = Bool
    -> NarrowableExpr
    -> Inference (T ())

concreteTypeOf :: (a, MGType) -> Inference Type
concreteTypeOf (_,Concrete x) = return x
concreteTypeOf _              = mempty

hasInferredType :: Ident Resolved -> Type -> T ()
hasInferredType ident newType = curFacts >>= setCurFacts . addIdentTypeFact ident newType

narrowingRules :: [NarrowingRule]
narrowingRules =
    [ isEqualNarrowing "isEqualTo" True True id
    , isEqualNarrowing "isEqualTo" False True id
    , isEqualNarrowing "isEqualType" True True Type.simpleTypesOf
    , isEqualNarrowing "isEqualType" False True Type.simpleTypesOf
    , isEqualNarrowing "isNotEqualTo" True False id
    , isEqualNarrowing "isNotEqualTo" False False id
    ]

isEqualNarrowing :: String -> Bool -> Bool -> (Type -> Type) -> NarrowingRule
isEqualNarrowing commandName isFlippedArgs whenGuessIs typeF expected expr = do
    guard (expected==whenGuessIs)
    (cmd, l, r) <- isSQFBinaryCommandCall expr
    let (l', r') = if isFlippedArgs then (r,l) else (l,r)
    guard $ cmd == commandName
    ident <- isVariable $ notTopLevel l'
    lType <- concreteTypeOf l'
    rType <- concreteTypeOf r'
    return $ fst ident `hasInferredType` (lType `Type.intersect` typeF rType)

notTopLevel :: (Expr_ Resolved, a)
    -> ExprF
     Type
     (Ident Resolved, ())
     ([Statement Resolved], ())
     (Expr_ Resolved)
notTopLevel = mapBlock (,()) . mapIdent (,()) . project . fst
