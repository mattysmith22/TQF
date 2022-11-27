{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module SQF.AST where

data Scope = Private | NoPrivate
    deriving (Show, Eq)

data SQFType = SStmt | SExpr

data SQF (a :: SQFType) where
    Variable :: String -> SQF a
    BinOp :: String -> SQF SExpr -> SQF SExpr -> SQF a
    UnOp :: String -> SQF SExpr -> SQF a
    NulOp :: String -> SQF a
    NumLit :: Double -> SQF a
    Array :: [SQF SExpr] -> SQF a
    StringLit :: String -> SQF a
    BoolLit :: Bool -> SQF a
    CodeBlock :: [SQF SStmt] -> SQF a
    Assign :: Scope -> String -> SQF SExpr -> SQF SStmt

forceExpr :: SQF a -> SQF SExpr
forceExpr x@Assign{} = UnOp "call" $ CodeBlock [x]
forceExpr (Variable x) = Variable x
forceExpr (BinOp c l r) = BinOp c l r
forceExpr (UnOp c x) = UnOp c x
forceExpr (NulOp c) = NulOp c
forceExpr (NumLit x) = NumLit x
forceExpr (Array xs) = Array xs
forceExpr (StringLit x) = StringLit x
forceExpr (BoolLit x) = BoolLit x
forceExpr (CodeBlock xs) = CodeBlock xs

forceStmt :: SQF a -> SQF SStmt
forceStmt x@Assign{} = x
forceStmt (Variable x) = Variable x
forceStmt (BinOp c l r) = BinOp c l r
forceStmt (UnOp c x) = UnOp c x
forceStmt (NulOp c) = NulOp c
forceStmt (NumLit x) = NumLit x
forceStmt (Array xs) = Array xs
forceStmt (StringLit x) = StringLit x
forceStmt (BoolLit x) = BoolLit x
forceStmt (CodeBlock xs) = CodeBlock xs

deriving instance Eq (SQF SExpr)
deriving instance Eq (SQF SStmt)
deriving instance Show (SQF SExpr)
deriving instance Show (SQF SStmt)

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

instance PrettyPrint [SQF SStmt] where
    prettyPrintPrec indent p xs = intercalate (newline indent) ((\x -> prettyPrintPrec indent p x . showString ";") <$> xs)

instance PrettyPrint (SQF a) where
    prettyPrintPrec indent _ (Assign Private name expr)
        = showString "private "
        . showString name
        . showString " = "
        . prettyPrintPrec indent 0 expr
    prettyPrintPrec indent _ (Assign NoPrivate name expr)
        = showString name 
        . showString " = "
        . prettyPrintPrec indent 0 expr
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