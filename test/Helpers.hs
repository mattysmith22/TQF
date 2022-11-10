module Helpers where

import qualified Data.Map                      as Map
import           TQF.AST
import           TQF.ModuleResolver

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

data NamespaceLiteral = NamespaceLiteral [(String, [Declaration])]
                                         [(String, Either NamespaceLiteral TypeName)]

buildNamespace :: NamespaceLiteral -> Namespace
buildNamespace = namespace' []
 where
  namespace' path (NamespaceLiteral lowerIdents upperIdents) = Namespace path
                                                                         lowerIdents'
                                                                         upperIdents'
   where
    lowerIdents' = Map.fromList $ map (mapFst l) lowerIdents
    upperIdents' = Map.fromList $ map upperIdentFunc upperIdents
    upperIdentFunc (ident, Right typ ) = (u ident, Right typ)
    upperIdentFunc (ident, Left child) = (u ident, Left $ namespace' (path ++ [u ident]) child)
