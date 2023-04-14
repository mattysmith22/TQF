module TQF.TypeCheck.Types
    ( TypeCheckErr(..)
    , T
    , MGType(..)
    ) where

import           Data.String.Pretty
import           TQF.AST
import           TQF.AST.Annotated
import           TQF.Type            (GenericType, Type)
import           TQF.TypeCheck.Facts
import           TQF.TypeCheck.Monad

data TypeCheckErr = NotWithin (Annot Type) (Annot Type)
     | NoField (Annot VarName) (Annot Type)
     | InvalidBinOp (Annot BinaryOperator) Type Type
     | InvalidUnOp (Annot UnaryOperator) Type
     | ExpectedCode Range
     | NotFound (Annot UIdent)
     | CouldntInferType (Annot GenericType)
     deriving (Show, Eq)

type T a = TypeCheck Facts Type (Either TypeCheckErr) a

instance Pretty TypeCheckErr where
    prettyPrint (NotWithin l r) = "type\n" ++ prettyPrint l ++ "\nis not within type\n" ++ prettyPrint r
    prettyPrint (NoField name typ) = "type\n" ++ prettyPrint typ ++ "\n does not have a field " ++ prettyPrint name
    prettyPrint (InvalidBinOp op l r) = "Invalid arguments to binary operator " ++ prettyPrint op ++ ": " ++ prettyPrint l ++ " and " ++ prettyPrint r
    prettyPrint (InvalidUnOp op x) = "Invalid argument to unary operator " ++ prettyPrint op ++ ": " ++ prettyPrint x
    prettyPrint (ExpectedCode r) = "Expected code " ++ prettyPrint r
    prettyPrint (NotFound x) = "Not found: " ++ prettyPrint x
    prettyPrint (CouldntInferType x) = "Couldn't infer type at " ++ prettyPrint (pos x)

data MGType
    = Generic GenericType
    | Concrete Type
    deriving (Show, Eq)
