{-# LANGUAGE FlexibleInstances #-}
module SQF.Commands where
import           Data.List
import           Data.String.CaseInsensitive

data Type = Array
    | Bool
    | Code
    | Config
    | Control
    | Diary
    | Display
    | Group
    | HashMap
    | Location
    | Number
    | Object
    | ScriptHandle
    | Side
    | String
    | StructuredText
    | Task
    | Nil
    deriving (Show, Eq)

data Command argTypes = Command
  { commArgTypes :: argTypes
  , commName     :: String
  , commRetType  :: Type
  }
type NulOpArg = ()
type UnOpArg = Type
type BinOpArg = (Type, Type)

type NulOp = Command NulOpArg
type UnOp = Command UnOpArg
type BinOp = Command BinOpArg

class Eq x => ArgType x where
    allCommands :: [Command x]

searchFor :: ArgType args => String -> args -> Maybe (Command args)
searchFor name args = find p allCommands
  where p command = commName command =~= name && commArgTypes command == args

searchForType :: ArgType args => String -> args -> Maybe Type
searchForType name args = commRetType <$> searchFor name args

instance ArgType (Type,Type) where
  allCommands = [Command (Object, Array) "setPos" Nil]

instance ArgType Type where
  allCommands = [Command Object "getPos" Array]

instance ArgType () where
  allCommands = [Command () "allUnits" Array]
