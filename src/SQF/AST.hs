{-# LANGUAGE FlexibleInstances #-}
module SQF.AST where

data Statement = Assign Scope String Expr
    | Expr Expr
    deriving (Show, Eq)

data Scope = Private | NoPrivate
    deriving (Show, Eq)

data Expr = Variable String
    | BinOp String Expr Expr
    | UnOp String Expr
    | NulOp String
    | NumLit Double
    | Array [Expr]
    | StringLit String
    | BoolLit Bool
    | CodeBlock [Statement]
    deriving (Show, Eq)

newline :: Int -> ShowS
newline x = showString $ "\n" ++ replicate (x*4) ' '

intercalate :: ShowS -> [ShowS] -> ShowS
intercalate _ [] = id
intercalate _ [x] = x
intercalate delim (x:xs) = x . delim . intercalate delim xs

prettyPrint :: PrettyPrint a => a -> String
prettyPrint x = prettyPrintPrec 0 0 x ""

class PrettyPrint a where
    prettyPrintPrec :: Int -> Int -> a -> ShowS

instance PrettyPrint [Statement] where
    prettyPrintPrec indent p xs = intercalate (newline indent) ((\x -> prettyPrintPrec indent p x . showString ";") <$> xs)

instance PrettyPrint Statement where
    prettyPrintPrec indent _ (Assign Private name expr)
        = showString "private "
        . showString name
        . showString " = "
        . prettyPrintPrec indent 0 expr
    prettyPrintPrec indent _ (Assign NoPrivate name expr)
        = showString name 
        . showString " = "
        . prettyPrintPrec indent 0 expr
    prettyPrintPrec indent _ (Expr x) = prettyPrintPrec indent 0 x

instance PrettyPrint Expr where
    prettyPrintPrec _ _ (Variable x) = showString x
    prettyPrintPrec indent p (BinOp name l r) = let
        opPrec = precedenceOfBinOp name
        in showParen (p > opPrec)
            $ prettyPrintPrec indent opPrec l
            . showString " "
            . showString name
            . showString " "
            . prettyPrintPrec indent opPrec r
    prettyPrintPrec indent p (UnOp name x) = let
        opPrec = precedenceOfUnaryOp
        in showParen (p > opPrec)
            $ showString name
            . showString " "
            . prettyPrintPrec indent opPrec x
    prettyPrintPrec indent p (NulOp name) = showString name
    prettyPrintPrec indent p (NumLit x) = showString (show x)
    prettyPrintPrec indent p (StringLit x) = showString "\"" . showString x . showString "\"" --TODO: Better string escaping
    prettyPrintPrec indent p (BoolLit True) = showString "true"
    prettyPrintPrec indent p (BoolLit False) = showString "false"
    prettyPrintPrec indent p (Array xs) = showString "[" . intercalate (showString ", ") (prettyPrintPrec indent p <$> xs) . showString "]" 
    prettyPrintPrec indent p (CodeBlock xs) = let
        i' = indent + 1
        in showString "{" . newline i'
            . intercalate (newline i') (fmap (\x -> prettyPrintPrec i' 0 x . showString ";") xs)
            . newline indent
            . showString "}"

precedenceOfBinOp :: String -> Int
precedenceOfBinOp "#" = 9
precedenceOfBinOp "^" = 8
precedenceOfBinOp "*" = 7
precedenceOfBinOp "/" = 7
precedenceOfBinOp "&" = 7
precedenceOfBinOp "mod" = 7
precedenceOfBinOp "atan2" = 7
precedenceOfBinOp "+" = 6
precedenceOfBinOp "-" = 6
precedenceOfBinOp "min" = 6
precedenceOfBinOp "max" = 6
precedenceOfBinOp "else" = 5
precedenceOfBinOp "==" = 3
precedenceOfBinOp "!=" = 3
precedenceOfBinOp ">" = 3
precedenceOfBinOp ">=" = 3
precedenceOfBinOp "<" = 3
precedenceOfBinOp "<=" = 3
precedenceOfBinOp ">>" = 3
precedenceOfBinOp "&&" = 2
precedenceOfBinOp "and" = 2
precedenceOfBinOp "||" = 2
precedenceOfBinOp "or" = 2
precedenceOfBinOp _ = 4

precedenceOfUnaryOp :: Int
precedenceOfUnaryOp = 10

precedenceOfNulOp :: Int
precedenceOfNulOp = 11