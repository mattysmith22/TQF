module Data.String.CaseInsensitive where
import           Data.Char

caseInsensEq :: String -> String -> Bool
caseInsensEq x y = map toLower x == map toLower y

(=~=) :: String -> String -> Bool
(=~=) = caseInsensEq
