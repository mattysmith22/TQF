module TQF.ToSQF.Type where

import qualified SQF.Commands                  as SQF
import           TQF.AST

toSQFType :: Type -> Maybe SQF.Type
toSQFType (Type [] (TypeName name)) = toSQFTypeName name
toSQFType _                         = Nothing

toSQFTypeName :: String -> Maybe SQF.Type
toSQFTypeName "Num"    = Just SQF.Number
toSQFTypeName "Object" = Just SQF.Object
toSQFTypeName "Array"  = Just SQF.Array
toSQFTypeName _        = Nothing

fromSQFTypeName :: SQF.Type -> String
fromSQFTypeName = show

fromSQFType :: SQF.Type -> Type
fromSQFType = Type [] . TypeName . fromSQFTypeName
