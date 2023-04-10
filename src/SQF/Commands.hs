{-# LANGUAGE FlexibleInstances #-}
module SQF.Commands
  ( CommandArgs(..)
  ) where

data CommandArgs a = CommandNular
  | CommandUnary a
  | CommandBinary a a
  deriving (Show, Eq)

instance Functor CommandArgs where
  fmap _ CommandNular        = CommandNular
  fmap f (CommandUnary x)    = CommandUnary (f x)
  fmap f (CommandBinary x y) = CommandBinary (f x) (f y)
instance Foldable CommandArgs where
  foldr _ acc CommandNular        = acc
  foldr f acc (CommandUnary x)    = f x acc
  foldr f acc (CommandBinary x y) = f x (f y acc)

instance Traversable CommandArgs where
  traverse _ CommandNular        = pure CommandNular
  traverse f (CommandUnary x)    = CommandUnary <$> f x
  traverse f (CommandBinary x y) = CommandBinary <$> f x <*> f y
