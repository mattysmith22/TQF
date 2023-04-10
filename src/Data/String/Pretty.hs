module Data.String.Pretty
    ( Pretty(..)
    ) where

class Pretty a where
    prettyPrint :: a -> String
