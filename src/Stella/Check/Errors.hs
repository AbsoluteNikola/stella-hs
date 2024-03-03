module Stella.Check.Errors (mkError, ErrorType(..), StellaError, renderStellaError) where

import Stella.Ast.AbsSyntax (HasPosition (hasPosition))
import Data.Text (Text)
import qualified Data.Text as T
import Stella.Ast.PrintSyntax (Print, printTree)

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
  = ErrorUnexpectedTypeForExpression --  {- Should be -} SType {- actual -} SType
  | ErrorMissingMain
  | ErrorUndefinedVariable -- {- name of undefined variable -} Text
  | ErrorNotAFunction
  | ErrorNotATuple
  | ErrorNotARecord
  | ErrorNotAList
  | ErrorNUnexpectedLambda
  | ErrorNUnexpectedTypeForAParameter
  | ErrorNUnexpectedTuple
  | ErrorNUnexpectedRecord
  | ErrorNUnexpectedList
  | ErrorNUnexpectedInjection
  | ErrorNMissingRecordFields
  | ErrorNUnexpectedRecordFields
  | ErrorNUnexpectedFieldAccess
  | ErrorTupleIndexOutOfBounds
  | ErrorNUnexpectedTupleLength
  | ErrorAmbiguousSumType
  | ErrorAmbiguousList
  | ErrorIllegalEmptyMatching
  | ErrorNonExhaustiveMatchPattern
  | ErrorUnexpectedPatternForType
  | ErrorUnimplementedCase

renderErrorType :: ErrorType -> Text
renderErrorType = \case
  ErrorUnexpectedTypeForExpression -> "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"
  ErrorMissingMain -> "ERROR_MISSING_MAIN"
  ErrorUndefinedVariable -> "ERROR_UNDEFINED_VARIABLE"
  ErrorNotAFunction -> "ERROR_NOT_A_FUNCTION"
  ErrorNotATuple -> "ERROR_NOT_A_TUPLE"
  ErrorNotARecord -> "ERROR_NOT_A_RECORD"
  ErrorNotAList -> "ERROR_NOT_A_LIST"
  ErrorNUnexpectedLambda -> "ERROR_UNEXPECTED_LAMBDA"
  ErrorNUnexpectedTypeForAParameter -> "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"
  ErrorNUnexpectedTuple -> "ERROR_UNEXPECTED_TUPLE"
  ErrorNUnexpectedRecord -> "ERROR_UNEXPECTED_RECORD"
  ErrorNUnexpectedList -> "ERROR_UNEXPECTED_LIST"
  ErrorNUnexpectedInjection -> "ERROR_UNEXPECTED_INJECTION"
  ErrorNMissingRecordFields -> "ERROR_MISSING_RECORD_FIELDS"
  ErrorNUnexpectedRecordFields -> "ERROR_UNEXPECTED_RECORD_FIELDS"
  ErrorNUnexpectedFieldAccess -> "ERROR_UNEXPECTED_FIELD_ACCESS"
  ErrorTupleIndexOutOfBounds -> "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS"
  ErrorNUnexpectedTupleLength -> "ERROR_UNEXPECTED_TUPLE_LENGTH"
  ErrorAmbiguousSumType -> "ERROR_AMBIGUOUS_SUM_TYPE"
  ErrorAmbiguousList -> "ERROR_AMBIGUOUS_LIST"
  ErrorIllegalEmptyMatching -> "ERROR_ILLEGAL_EMPTY_MATCHING"
  ErrorNonExhaustiveMatchPattern -> "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"
  ErrorUnexpectedPatternForType -> "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"
  ErrorUnimplementedCase -> "ERROR_UNIMPLEMENTED_CASE"

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
