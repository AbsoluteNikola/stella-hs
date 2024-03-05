module Stella.Check.Errors (mkError, ErrorType(..), StellaError, renderStellaError) where

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
  | ErrorUnexpectedFieldAccess
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
  | ErrorUnexpectedNonNullaryVariantPattern
  | ErrorUnexpectedNullaryVariantPattern

renderErrorType :: ErrorType -> Text
renderErrorType = \case
  ErrorUnexpectedTypeForExpression actual needed ->
    "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: \n" <>
    "Expected type: " <> pp needed <> "\n" <>
    "But got: " <> pp actual
  ErrorUnexpectedTypeForExpressionText text -> "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: " <> text
  ErrorMissingMain -> "ERROR_MISSING_MAIN"
  ErrorUndefinedVariable -> "ERROR_UNDEFINED_VARIABLE"
  ErrorNotAFunction -> "ERROR_NOT_A_FUNCTION"
  ErrorNotATuple -> "ERROR_NOT_A_TUPLE"
  ErrorNotARecord -> "ERROR_NOT_A_RECORD"
  ErrorNotAList -> "ERROR_NOT_A_LIST"
  ErrorUnexpectedLambda -> "ERROR_UNEXPECTED_LAMBDA"
  ErrorUnexpectedTypeForAParameter -> "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"
  ErrorUnexpectedTuple -> "ERROR_UNEXPECTED_TUPLE"
  ErrorUnexpectedRecord -> "ERROR_UNEXPECTED_RECORD"
  ErrorUnexpectedList -> "ERROR_UNEXPECTED_LIST"
  ErrorUnexpectedInjection -> "ERROR_UNEXPECTED_INJECTION"
  ErrorMissingRecordFields name -> "ERROR_MISSING_RECORD_FIELDS: " <> name
  ErrorUnexpectedRecordFields -> "ERROR_UNEXPECTED_RECORD_FIELDS"
  ErrorUnexpectedFieldAccess -> "ERROR_UNEXPECTED_FIELD_ACCESS"
  ErrorTupleIndexOutOfBounds index -> "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS: " <> T.pack (show index)
  ErrorUnexpectedTupleLength -> "ERROR_UNEXPECTED_TUPLE_LENGTH"
  ErrorAmbiguousSumType -> "ERROR_AMBIGUOUS_SUM_TYPE"
  ErrorAmbiguousList -> "ERROR_AMBIGUOUS_LIST"
  ErrorIllegalEmptyMatching -> "ERROR_ILLEGAL_EMPTY_MATCHING"
  ErrorNonExhaustiveMatchPattern -> "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"
  ErrorUnexpectedPatternForType -> "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"
  ErrorUnimplementedCase -> "ERROR_UNIMPLEMENTED_CASE"
  ErrorDuplicateRecordFields names -> "ERROR_DUPLICATE_RECORD_FIELDS: " <> T.intercalate ", " names
  ErrorDuplicatePatternVariable names -> "ERROR_DUPLICATE_PATTERN_VARIABLE" <> T.intercalate ", " names
  ErrorAmbiguousVariantType -> "ERROR_AMBIGUOUS_VARIANT_TYPE"
  ErrorUnexpectedVariantLabel name -> "ERROR_UNEXPECTED_VARIANT_LABEL: " <> name
  ErrorUnexpectedNonNullaryVariantPattern -> "ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN: "
  ErrorUnexpectedNullaryVariantPattern -> "ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN: "

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
