module Data.String.Pretty where

class Pretty a where
    prettyPrint :: a -> String