module Stella.Check.Types where
import Data.Text

data FuncTypeData = FuncTypeData
  { argsType :: [SType]
  , returnType :: SType
  } deriving (Eq, Show)

data SimpleType
  = Unit
  | Boolean
  | Nat
  deriving (Eq, Show)

data SType
  = FuncType FuncTypeData
  | ListType SType
  | SimpleType SimpleType
  | TypeVarType Text -- type with name should be in env
  deriving (Eq, Show)

unit_, bool_, nat_ :: SType
unit_ = SimpleType Unit
bool_ = SimpleType Boolean
nat_ = SimpleType Nat
