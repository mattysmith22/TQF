{-# LANGUAGE RecordWildCards #-}
module TQF.VNI where

import TQF.AST
import qualified Data.Bimap as Bimap
import Data.Char
import Data.List

escapedChars :: Bimap.Bimap Char Char 
escapedChars = Bimap.fromList 
    [ ('<', 'o')
    , ('>', 't')
    , (',', 'c')
    , ('.', 'd')
    , ('_', 'u')
    ]

escapeString :: String -> String
escapeString = concatMap f
    where
        f x | isAlphaNum x = [x]
            | otherwise = case Bimap.lookup x escapedChars of
                Nothing -> ""
                (Just x) -> "_" ++ [x]

unescapeString :: String -> String
unescapeString = filter (/=' ') . snd . mapAccumL f False
    where
        f False '_' = (True, ' ')
        f False x = (False, x)
        f True x = case Bimap.lookupR x escapedChars of
            Nothing -> (False, ' ')
            (Just x) -> (False, x)

vniForModule :: ResolveableModule -> String
vniForModule = concatMap ((++"_") . unTypeName)

vniForDecl :: ResolveableModule -> Declaration -> String
vniForDecl modl VariableDecl{..} = vniForModule modl ++ unVarName variableName
vniForDecl modl FunctionDecl{..} = vniForModule modl
    ++ "fnc_"
    ++ unVarName functionName
    ++ typeDecl
    where
        typeToString (Type [] v) = unTypeName v
        typeToString (Type xs v) = vniForModule xs ++ "_" ++ unTypeName v

        typeDecl = if QualifierExtern `elem` functionQualifiers then
                ""
            else
                "__" ++ intercalate "_" (fmap (typeToString . fst) functionArguments)