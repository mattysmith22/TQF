{-# LANGUAGE FlexibleInstances #-}
module TQF.Types
    ( ToIdent(..)
    , VarName(..)
    , TypeName(..)
    ) where

import           Data.List.Extra    (intercalate)
import           Data.String.Pretty

class ToIdent a where
  toIdent :: String -> a

newtype VarName = VarName {unVarName:: String}
    deriving (Eq, Ord)

instance Pretty VarName where
  prettyPrint = unVarName

instance Pretty [VarName] where
  prettyPrint = intercalate "." . fmap prettyPrint

instance ToIdent VarName where
  toIdent = VarName

newtype TypeName = TypeName {unTypeName:: String}
    deriving (Eq, Ord)

instance Pretty TypeName where
  prettyPrint = unTypeName

instance Pretty [TypeName] where
  prettyPrint = intercalate "." . fmap prettyPrint

instance ToIdent TypeName where
  toIdent = TypeName

instance Show TypeName where
  show (TypeName x) = show x

instance Show VarName where
  show (VarName x) = show x
