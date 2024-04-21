module Stella.Check.Errors where

import Stella.Ast.AbsSyntax (HasPosition (hasPosition))
import Data.Text (Text)
import qualified Data.Text as T
import Stella.Ast.PrintSyntax (Print, printTree)
import Stella.Check.Types (SType)
import Stella.Check.Utils (pp)

data StellaError where
  StellaError :: (HasPosition s, Print s) =>
    { brokenNode :: s
    , errorType :: ErrorType
    } -> StellaError

instance Show StellaError where
  show = T.unpack . renderStellaError

mkError :: (HasPosition s, Print s) => s -> ErrorType -> StellaError
mkError node err = StellaError
  { brokenNode = node
  , errorType = err
  }

data ErrorType
  = ErrorUnexpectedTypeForExpression {- actual -} SType {- needed -} SType
  | ErrorUnexpectedTypeForExpressionText Text
  | ErrorNotAFunctionText Text
  | ErrorMissingMain
  | ErrorUndefinedVariable -- {- name of undefined variable -} Text
  | ErrorNotAFunction
  | ErrorNotATuple
  | ErrorNotARecord
  | ErrorNotAList
  | ErrorUnexpectedLambda
  | ErrorUnexpectedTypeForAParameter
  | ErrorUnexpectedTuple
  | ErrorUnexpectedRecord
  | ErrorUnexpectedList
  | ErrorUnexpectedInjection
  | ErrorMissingRecordFields Text
  | ErrorUnexpectedRecordFields
  | ErrorUnexpectedFieldAccess Text
  | ErrorTupleIndexOutOfBounds Integer
  | ErrorUnexpectedTupleLength
  | ErrorAmbiguousSumType
  | ErrorAmbiguousList
  | ErrorAmbiguousVariantType
  | ErrorIllegalEmptyMatching
  | ErrorNonExhaustiveMatchPattern
  | ErrorUnexpectedPatternForType
  | ErrorUnimplementedCase
  | ErrorDuplicateRecordFields [Text]
  | ErrorDuplicatePatternVariable [Text]
  | ErrorUnexpectedVariantLabel Text
  | ErrorUnexpectedVariant (Maybe SType)
  | ErrorMissingDataForLabel Text
  | ErrorUnexpectedNonNullaryVariantPattern
  | ErrorUnexpectedNullaryVariantPattern
  | ErrorIncorrectArityOfMain Int
  | ErrorIncorrectNumberOfArguments Int
  | ErrorUnexpectedDataForNullaryLabel SType
  | ErrorAmbiguousPatternType Text
  | ErrorExceptionTypeNotDeclared
  | ErrorAmbiguousThrowType
  | ErrorAmbiguousReferenceType
  | ErrorAmbiguousPanicType
  | ErrorNotAReference
  | ErrorUnexpectedMemoryAddress
  | ErrorUnexpectedSubtype {- actual -} SType {- excpected -} SType

renderErrorTypeOnlyCode ::  ErrorType -> Text
renderErrorTypeOnlyCode = \case
  ErrorUnexpectedTypeForExpression{} -> "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"
  ErrorUnexpectedTypeForExpressionText{} -> "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"
  ErrorMissingMain -> "ERROR_MISSING_MAIN"
  ErrorUndefinedVariable -> "ERROR_UNDEFINED_VARIABLE"
  ErrorNotAFunction -> "ERROR_NOT_A_FUNCTION"
  ErrorNotAFunctionText{} -> "ERROR_NOT_A_FUNCTION"
  ErrorNotATuple -> "ERROR_NOT_A_TUPLE"
  ErrorNotARecord -> "ERROR_NOT_A_RECORD"
  ErrorNotAList -> "ERROR_NOT_A_LIST"
  ErrorUnexpectedLambda -> "ERROR_UNEXPECTED_LAMBDA"
  ErrorUnexpectedTypeForAParameter -> "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"
  ErrorUnexpectedTuple -> "ERROR_UNEXPECTED_TUPLE"
  ErrorUnexpectedRecord -> "ERROR_UNEXPECTED_RECORD"
  ErrorUnexpectedList -> "ERROR_UNEXPECTED_LIST"
  ErrorUnexpectedInjection -> "ERROR_UNEXPECTED_INJECTION"
  ErrorMissingRecordFields{} -> "ERROR_MISSING_RECORD_FIELDS"
  ErrorUnexpectedRecordFields -> "ERROR_UNEXPECTED_RECORD_FIELDS"
  ErrorUnexpectedFieldAccess{} -> "ERROR_UNEXPECTED_FIELD_ACCESS"
  ErrorTupleIndexOutOfBounds{} -> "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS"
  ErrorUnexpectedTupleLength -> "ERROR_UNEXPECTED_TUPLE_LENGTH"
  ErrorAmbiguousSumType -> "ERROR_AMBIGUOUS_SUM_TYPE"
  ErrorAmbiguousList -> "ERROR_AMBIGUOUS_LIST"
  ErrorIllegalEmptyMatching -> "ERROR_ILLEGAL_EMPTY_MATCHING"
  ErrorNonExhaustiveMatchPattern -> "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"
  ErrorUnexpectedPatternForType -> "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"
  ErrorUnimplementedCase -> "ERROR_UNIMPLEMENTED_CASE"
  ErrorDuplicateRecordFields{} -> "ERROR_DUPLICATE_RECORD_FIELDS"
  ErrorDuplicatePatternVariable{} -> "ERROR_DUPLICATE_PATTERN_VARIABLE"
  ErrorAmbiguousVariantType -> "ERROR_AMBIGUOUS_VARIANT_TYPE"
  ErrorUnexpectedVariant{} -> "ERROR_UNEXPECTED_VARIANT"
  ErrorUnexpectedVariantLabel{} -> "ERROR_UNEXPECTED_VARIANT_LABEL"
  ErrorMissingDataForLabel{} -> "ERROR_MISSING_DATA_FOR_LABEL"
  ErrorUnexpectedNonNullaryVariantPattern -> "ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN"
  ErrorUnexpectedNullaryVariantPattern -> "ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN"
  ErrorIncorrectArityOfMain{} -> "ERROR_INCORRECT_ARITY_OF_MAIN"
  ErrorIncorrectNumberOfArguments{} -> "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS"
  ErrorUnexpectedDataForNullaryLabel{} -> "ERROR_UNEXPECTED_DATA_FOR_NULLARY_LABEL"
  ErrorAmbiguousPatternType{} -> "ERROR_AMBIGUOUS_PATTERN_TYPE"
  ErrorExceptionTypeNotDeclared -> "ERROR_EXCEPTION_TYPE_NOT_DECLARED"
  ErrorAmbiguousThrowType -> "ERROR_AMBIGUOUS_THROW_TYPE"
  ErrorAmbiguousReferenceType -> "ERROR_AMBIGUOUS_REFERENCE_TYPE"
  ErrorAmbiguousPanicType -> "ERROR_AMBIGUOUS_PANIC_TYPE"
  ErrorNotAReference -> "ERROR_NOT_A_REFERENCE"
  ErrorUnexpectedMemoryAddress -> "ERROR_UNEXPECTED_MEMORY_ADDRESS"
  ErrorUnexpectedSubtype{} -> "ERROR_UNEXPECTED_SUBTYPE"

renderErrorType :: ErrorType -> Text
renderErrorType t = case t of
  ErrorUnexpectedTypeForExpression actual needed ->
    renderErrorTypeOnlyCode t <> ": " <>
    "Expected type: " <> pp needed <> "\n" <>
    "But got: " <> pp actual
  ErrorUnexpectedSubtype actual needed ->
    renderErrorTypeOnlyCode t <> ": " <>
    "Expected type: " <> pp needed <> "\n" <>
    "But got: " <> pp actual
  ErrorUnexpectedTypeForExpressionText text -> renderErrorTypeOnlyCode t <> ": " <> text
  ErrorNotAFunctionText text -> renderErrorTypeOnlyCode t <> ": " <> text
  ErrorMissingRecordFields name -> renderErrorTypeOnlyCode t <> ": " <> name
  ErrorTupleIndexOutOfBounds index -> renderErrorTypeOnlyCode t <> ": " <> T.pack (show index)
  ErrorDuplicateRecordFields names -> renderErrorTypeOnlyCode t <> ": " <> T.intercalate ", " names
  ErrorDuplicatePatternVariable names -> renderErrorTypeOnlyCode t <> ": " <> T.intercalate ", " names
  ErrorUnexpectedVariant Nothing -> renderErrorTypeOnlyCode t
  ErrorUnexpectedVariant (Just type_) -> renderErrorTypeOnlyCode t <> ": " <> pp type_
  ErrorUnexpectedVariantLabel name -> renderErrorTypeOnlyCode t <> ": " <> name
  ErrorIncorrectArityOfMain n -> renderErrorTypeOnlyCode t <> ": " <> pp n
  ErrorIncorrectNumberOfArguments n -> renderErrorTypeOnlyCode t <> ": " <> pp n
  ErrorUnexpectedFieldAccess n -> renderErrorTypeOnlyCode t <> ": " <> pp n
  ErrorMissingDataForLabel n -> renderErrorTypeOnlyCode t <> ": " <> pp n
  ErrorUnexpectedDataForNullaryLabel type_ -> renderErrorTypeOnlyCode t <> ": " <> pp type_
  ErrorAmbiguousPatternType text -> renderErrorTypeOnlyCode t <> ": " <> pp text
  _ -> renderErrorTypeOnlyCode t

renderStellaError :: StellaError -> Text
renderStellaError StellaError{..} = "Error: " <>
  renderErrorType errorType <> "\n" <>
  renderPosition (hasPosition brokenNode) <> "\n" <>
  "Broken node: " <> "\n" <>
  T.pack (printTree brokenNode)

renderPosition :: Maybe (Int, Int) -> Text
renderPosition = \case
  Nothing -> ""
  Just (line, col) -> "At position " <> renderInt line <> ":" <> renderInt col

renderInt :: Int -> Text
renderInt = T.pack . show
