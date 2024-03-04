module Stella.Check.Types where
import Data.Text
import qualified Data.Map as Map

data FuncTypeData = FuncTypeData
  { argsType :: [SType]
  , returnType :: SType
  } deriving (Eq, Show)

newtype RecordTypeData = RecordTypeData
  { recordFields :: Map.Map Text SType
  } deriving (Eq, Show)

newtype VariantTypeData = VariantTypeData
  { variants :: Map.Map Text (Maybe SType) -- Nothing when variant case used like enum
  } deriving (Eq, Show)

newtype TupleTypeData = TupleTypeData
  { tupleTypes :: [SType]
  } deriving (Eq, Show)

data SumTypeData = SumTypeData
  { leftType :: SType
  , rightType :: SType
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
  | TupleType TupleTypeData
  | RecordType RecordTypeData
  | SumType SumTypeData
  | VariantType VariantTypeData
  deriving (Eq, Show)

unit_, bool_, nat_ :: SType
unit_ = SimpleType Unit
bool_ = SimpleType Boolean
nat_ = SimpleType Nat
