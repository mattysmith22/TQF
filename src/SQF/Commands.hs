{-# LANGUAGE FlexibleInstances #-}
module SQF.Commands where
import           Data.List
import           Data.String.CaseInsensitive

data CommandArgs a = CommandNular
  | CommandUnary a
  | CommandBinary a a
  deriving (Show, Eq)