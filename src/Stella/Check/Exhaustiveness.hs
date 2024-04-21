module Stella.Check.Exhaustiveness
  ( checkPatternsExhaustive
  ) where
import qualified Data.List.NonEmpty as NE
import Stella.Ast.AbsSyntax
import Data.Foldable (find, Foldable (foldl'))
import Data.Maybe (isJust)
import Stella.Check.Types (SType (VariantType), VariantTypeData (..))
import qualified Data.Map as Map

-- | Check that there are patterns for all cases of top level type.
-- Waring:  Should be called only after type check after
checkPatternsExhaustive :: SType -> NE.NonEmpty Pattern -> Bool
checkPatternsExhaustive type_ patterns =
  let
    hasVarPattern = find' patterns $ \case
      PatternVar{} -> True
      _ -> False
  in hasVarPattern || checkPatternsByType type_ patterns

checkPatternsByType :: SType -> NE.NonEmpty Pattern -> Bool
checkPatternsByType type_ patterns = case NE.head patterns of
  PatternVariant{} -> checkVariantPatterns type_ patterns
  PatternInl{} -> checkSumTypePatterns patterns
  PatternInr{} -> checkSumTypePatterns patterns
  PatternAsc{} -> True -- no support for structure patterns
  PatternList{} -> True -- no support for structure patterns
  PatternTuple{} -> True -- no support for structure patterns
  PatternRecord{} -> True -- no support for structure patterns
  PatternCons{} -> undefined
  PatternFalse{} -> checkBooleanPatterns patterns
  PatternTrue{} -> checkBooleanPatterns patterns
  PatternUnit{} -> True --  only one constructor and it's already here. Other may be only only Var pattern or another units
  PatternInt{} -> checkNatPatterns patterns
  PatternSucc{} -> checkNatPatterns patterns
  PatternVar{} -> True -- always exhaustive
  PatternCastAs{} -> True

checkNatPatterns :: NE.NonEmpty Pattern -> Bool
checkNatPatterns _ = False -- True only if there is a Var pattern and it checked on the top level

checkBooleanPatterns :: NE.NonEmpty Pattern -> Bool
checkBooleanPatterns patterns =
  let
    hasTruePattern = find' patterns $ \case
      PatternTrue{} -> True
      _ -> False
    hasFalsePattern = find' patterns $ \case
      PatternFalse{} -> True
      _ -> False
  in hasTruePattern && hasFalsePattern

checkSumTypePatterns :: NE.NonEmpty Pattern -> Bool
checkSumTypePatterns patterns =
  let
    hasInlPattern = find' patterns $ \case
      PatternInl{} -> True
      _ -> False
    hasInrPattern = find' patterns $ \case
      PatternInr{} -> True
      _ -> False
  in hasInlPattern && hasInrPattern

checkVariantPatterns :: SType -> NE.NonEmpty Pattern -> Bool
checkVariantPatterns type_ patterns = case type_ of
  VariantType (VariantTypeData vtd) ->
    let
      go cases pattern = case pattern of
        PatternVariant _ (StellaIdent name) _ -> Map.delete name cases
        _ -> cases
    in Map.null $ foldl' go vtd patterns
  _ -> error "Check variant patterns of not variant type impossible after type check"

find' :: Foldable f => f a -> (a -> Bool) -> Bool
find' ls f = isJust $ find f ls
