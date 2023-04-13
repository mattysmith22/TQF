{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TQF.Resolve.Types
    ( Resolved
    , LValue(..)
    ) where

import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Type

data Resolved

type instance TypeDeclF Resolved = Type
type instance LIdentF Resolved = ModLIdentDecl
type instance DeclIdentF Resolved = ModLIdentDecl
type instance LValueF Resolved = LValue Resolved

data LValue a
  = LValueVar (Annot (Ident a))
  | LValueField (Expr a) (Annot VarName)

deriving instance (Show (Ident a), Show (Expr a)) => Show (LValue a)
deriving instance (Eq (Ident a), Eq (Expr a)) => Eq (LValue a)
