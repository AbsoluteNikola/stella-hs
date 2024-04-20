module Stella.Check.Types where
import Data.Text
import qualified Data.Map as Map
import Stella.Check.Utils
import qualified Data.Text as T

data FuncTypeData = FuncTypeData
  { argsType :: [SType]
  , returnType :: SType
  } deriving (Eq, Show)

instance Pretty FuncTypeData where
  pp FuncTypeData{..}=
    "fn(" <>
    T.intercalate ", " (fmap pp argsType) <>
    ") -> " <>
    pp returnType

newtype RecordTypeData = RecordTypeData
  { recordFields :: [(Text, SType)]
  } deriving (Eq, Show)

instance Pretty RecordTypeData where
  pp RecordTypeData{..}=
    "{" <>
    T.intercalate ", "
      ((\(name, t) -> name <> " : " <> pp t) <$> recordFields) <>
    "}"

newtype VariantTypeData = VariantTypeData
  { variants :: Map.Map Text (Maybe SType) -- Nothing when variant case used like enum
  } deriving (Eq, Show)

instance Pretty VariantTypeData where
  pp VariantTypeData{..}=
    "<|" <>
    T.intercalate ", "
      ((\(name, t) -> name <> " : " <> maybe "" pp t) <$> Map.toList variants) <>
    "|>"

newtype TupleTypeData = TupleTypeData
  { tupleTypes :: [SType]
  } deriving (Eq, Show)

instance Pretty TupleTypeData where
  pp :: TupleTypeData -> Text
  pp TupleTypeData{..}=
    "{" <>
    T.intercalate ", " (pp <$>tupleTypes) <>
    "}"

data SumTypeData = SumTypeData
  { leftType :: SType
  , rightType :: SType
  } deriving (Eq, Show)

instance Pretty SumTypeData where
  pp :: SumTypeData -> Text
  pp SumTypeData{..} = pp leftType <> " + " <> pp rightType

data SimpleType
  = Unit
  | Boolean
  | Nat
  deriving (Eq, Show)

instance Pretty SimpleType where
  pp :: SimpleType -> Text
  pp = \case
    Unit -> "Unit"
    Boolean -> "Bool"
    Nat -> "Nat"

data SType
  = FuncType FuncTypeData
  | ListType SType
  | SimpleType SimpleType
  | TypeVarType Text -- type with name should be in env
  | TupleType TupleTypeData
  | RecordType RecordTypeData
  | SumType SumTypeData
  | VariantType VariantTypeData
  | RefType SType
  deriving (Eq, Show)

instance Pretty SType where
  pp = \case
    FuncType ftd -> pp ftd
    ListType t -> "[" <> pp t <> "]"
    SimpleType t -> pp t
    TypeVarType t -> "type " <> t
    TupleType t -> pp t
    RecordType t -> pp t
    SumType t -> pp t
    VariantType t -> pp t
    RefType t -> "&" <> pp t

unit_, bool_, nat_ :: SType
unit_ = SimpleType Unit
bool_ = SimpleType Boolean
nat_ = SimpleType Nat
