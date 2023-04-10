module Helpers where

import           TQF.AST

typN :: String -> UIdent
typN = UIdent [] . TypeName
varN :: String -> LIdent
varN = LIdent [] . VarName

typN' :: [String] -> String -> UIdent
typN' args = UIdent (map TypeName args) . TypeName
varN' :: [String] -> String -> LIdent
varN' args = LIdent (map TypeName args) . VarName

u :: String -> TypeName
u = TypeName
l :: String -> VarName
l = VarName

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
