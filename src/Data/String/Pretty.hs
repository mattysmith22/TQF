{-# LANGUAGE FlexibleInstances #-}
module Data.String.Pretty
    ( Pretty(..)
    ) where

class Pretty a where
    prettyPrint :: a -> String

instance Pretty String where
    prettyPrint = id
