module Stella.Check.Types where
import Data.Text (Text)
import qualified Data.Map as Map
import Stella.Check.Utils
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)

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
  { recordFields :: Map.Map Text SType
  } deriving (Eq, Show)

instance Pretty RecordTypeData where
  pp RecordTypeData{..}=
    "{" <>
    T.intercalate ", "
      ((\(name, t) -> name <> " : " <> pp t) <$> Map.toList recordFields) <>
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

data UniversalTypeData = UniversalTypeData
  { variables :: [Text]
  , innerType :: SType
  } deriving (Eq, Show)

instance Pretty UniversalTypeData where
  pp :: UniversalTypeData -> Text
  pp UniversalTypeData{..} = "forall " <> T.intercalate " " variables <> "." <> pp innerType


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
  | UniversalTypeVar Text
  | UniversalType UniversalTypeData
  | TupleType TupleTypeData
  | RecordType RecordTypeData
  | SumType SumTypeData
  | VariantType VariantTypeData
  | RefType SType
  | Top
  | Bottom
  | STypeVar Int
  deriving (Eq, Show)

unit_, bool_, nat_ :: SType
unit_ = SimpleType Unit
bool_ = SimpleType Boolean
nat_ = SimpleType Nat

instance Pretty SType where
  pp = \case
    FuncType ftd -> pp ftd
    ListType t -> "[" <> pp t <> "]"
    SimpleType t -> pp t
    UniversalTypeVar t -> t
    UniversalType utd  -> pp utd
    TupleType t -> pp t
    RecordType t -> pp t
    SumType t -> pp t
    VariantType t -> pp t
    RefType t -> "&" <> pp t
    Top -> "Top"
    Bottom -> "Bottom"
    STypeVar n -> "?T" <> pp n

eqWithSubtyping :: SType -> SType -> Bool

eqWithSubtyping (SimpleType actual) (SimpleType expected) = actual == expected

eqWithSubtyping (FuncType actual) (FuncType expected)
  | length actual.argsType /= length expected.argsType = False
  | otherwise =
      and (zipWith eqWithSubtyping expected.argsType actual.argsType)
      && eqWithSubtyping actual.returnType expected.returnType

eqWithSubtyping (ListType actual) (ListType expected) = eqWithSubtyping actual expected

eqWithSubtyping (TupleType actual) (TupleType expected)
  | length actual.tupleTypes /= length expected.tupleTypes = False
  | otherwise =
      and (zipWith eqWithSubtyping actual.tupleTypes expected.tupleTypes)

eqWithSubtyping
  (RecordType (RecordTypeData actual))
  (RecordType (RecordTypeData expected))
  = Map.isSubmapOfBy (flip eqWithSubtyping) expected actual

eqWithSubtyping (SumType actual) (SumType expected)
  =  (actual.leftType `eqWithSubtyping` expected.leftType)
  && (actual.rightType `eqWithSubtyping` expected.rightType)

eqWithSubtyping
  (VariantType (VariantTypeData actual))
  (VariantType (VariantTypeData expected))
  = Map.isSubmapOfBy f actual expected
    where
      f Nothing Nothing = True
      f (Just t1) (Just t2) = eqWithSubtyping t1 t2
      f _ _ = False

eqWithSubtyping (RefType actual) (RefType expected)
  = (actual `eqWithSubtyping` expected) && (expected `eqWithSubtyping` actual)

eqWithSubtyping Bottom _ = True
eqWithSubtyping _ Top = True

eqWithSubtyping actual expected
  | actual == expected = True
  | otherwise = False

recordFieldsMissing :: SType -> SType -> Bool
recordFieldsMissing
  (RecordType (RecordTypeData (Map.keysSet -> actual)))
  (RecordType (RecordTypeData (Map.keysSet -> expected)))
  = Set.isSubsetOf actual expected
recordFieldsMissing _ _ = False

tupleLengthDifference :: SType -> SType -> Bool
tupleLengthDifference (TupleType actual) (TupleType expected) =
  length actual.tupleTypes /= length expected.tupleTypes
tupleLengthDifference _ _ = False

variantUnexpectedLabel :: SType -> SType -> Bool
variantUnexpectedLabel
  (VariantType (VariantTypeData (Map.keysSet -> actual)))
  (VariantType (VariantTypeData (Map.keysSet -> expected))) =
    not $ Set.isSubsetOf actual expected
variantUnexpectedLabel _ _ = False

universalTypeSubstitute :: Map.Map Text SType -> SType -> SType
universalTypeSubstitute varsMapping = \case
  FuncType ftd -> FuncType FuncTypeData
    { argsType = map (universalTypeSubstitute varsMapping) ftd.argsType
    , returnType = universalTypeSubstitute varsMapping ftd.returnType
    }
  ListType t -> universalTypeSubstitute varsMapping t
  SimpleType t -> SimpleType t
  UniversalTypeVar t -> fromMaybe (UniversalTypeVar t) $ Map.lookup t varsMapping
  UniversalType utd  ->
    let
      substituted = universalTypeSubstitute varsMapping utd.innerType
      otherVars = mapMaybe
        (\name -> maybe (Just name) (const Nothing) $ Map.lookup name varsMapping)
        utd.variables
    in if null otherVars
      then substituted
      else UniversalType UniversalTypeData
        { variables = otherVars
        , innerType = substituted
        }
  TupleType t -> TupleType TupleTypeData
    { tupleTypes = map (universalTypeSubstitute varsMapping) t.tupleTypes
    }
  RecordType t -> RecordType RecordTypeData
    { recordFields = Map.map (universalTypeSubstitute varsMapping) t.recordFields
    }
  SumType t -> SumType SumTypeData
    { leftType = universalTypeSubstitute varsMapping t.leftType
    , rightType = universalTypeSubstitute varsMapping t.rightType
    }
  VariantType t -> VariantType VariantTypeData
    { variants = Map.map (>>= Just . universalTypeSubstitute varsMapping) t.variants
    }
  RefType t -> RefType $
    universalTypeSubstitute varsMapping t
  Top -> Top
  Bottom -> Bottom
  STypeVar n -> STypeVar n
