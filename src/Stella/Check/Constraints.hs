module Stella.Check.Constraints where
import Stella.Check.Types
import Stella.Check.Utils (Pretty(..))
import Stella.Ast.AbsSyntax (Expr)
import Data.Functor ((<&>))
import Control.Arrow (ArrowChoice(right))

data Constraint = Constraint
  { lc :: SType
  , rc :: SType
  , expr :: Expr
  } deriving (Eq, Show)

instance Pretty Constraint where
  pp Constraint{..} = pp lc <> " = " <> pp rc

data SolverError
  = InfinityType Constraint
  | SolvingFailed Constraint

solve :: [Constraint] -> Either SolverError [(SType, SType)]
solve [] = Right []
solve (c:cs) = case (c.lc, c.rc) of
  (tv1@STypeVar{}, tv2@STypeVar{})
    | tv1 == tv2 -> solve cs
  (tv1@STypeVar{}, t2)
    | containsIn tv1 t2 -> Left $ InfinityType c
    | otherwise -> right ((tv1, t2):) $ solve (substituteConstraints cs tv1 t2)
  (t1, tv2@STypeVar{})
    | containsIn tv2 t1 -> Left $ InfinityType c
    | otherwise -> right ((tv2, t1):) $ solve (substituteConstraints cs tv2 t1)
  (ListType t1, ListType t2) ->
      solve $ Constraint t1 t2 c.expr : cs
  (SumType t1, SumType t2) ->
      solve $ Constraint t1.leftType t2.leftType c.expr
        : Constraint t1.rightType t2.rightType c.expr
        : cs
  (TupleType ttd1, TupleType ttd2) ->
    solve $ cs
      ++ zipWith (\t1 t2 -> Constraint t1 t2 c.expr)
        ttd1.tupleTypes
        ttd2.tupleTypes
  (FuncType ftd1, FuncType ftd2) ->
    solve $ cs
      ++ [Constraint ftd1.returnType ftd2.returnType c.expr]
      ++ zipWith (\t1 t2 -> Constraint t1 t2 c.expr)
          ftd1.argsType ftd2.argsType
  (t1, t2)
    | eqWithSubtyping t1 t2 -> solve cs
  _ -> Left $ SolvingFailed c

substituteConstraints :: [Constraint] -> SType -> SType -> [Constraint]
substituteConstraints cs fromT toT = cs <&>
  \Constraint{..} -> Constraint
    { lc = substitute fromT toT lc
    , rc = substitute fromT toT rc
    , expr = expr
    }

substitute :: SType -> SType -> SType -> SType
substitute typeVar typ = \case
  tv@STypeVar{}
    | typeVar == tv -> typ
  FuncType ftd -> FuncType $ FuncTypeData
    { returnType = substitute typeVar typ ftd.returnType
    , argsType = ftd.argsType <&> \t -> substitute typeVar typ t
    }
  ListType t -> ListType $ substitute typeVar typ t
  TupleType ttd -> TupleType $ TupleTypeData $
    ttd.tupleTypes <&> \t -> substitute typeVar typ t
  RefType t -> substitute typeVar typ t
  t -> t


containsIn :: SType -> SType -> Bool
containsIn t1 = \case
  FuncType ftd ->
    containsIn t1 ftd.returnType || any (containsIn t1) ftd.argsType
  ListType t -> containsIn t1 t
  SimpleType _ -> False
  UniversalTypeVar _ -> False
  UniversalType utd -> containsIn t1 utd.innerType
  TupleType ttd -> any (containsIn t1) ttd.tupleTypes
  RecordType _ -> False
  SumType t -> containsIn t1 t.leftType || containsIn t1 t.rightType
  VariantType _ -> False
  RefType t -> containsIn t1 t
  Top -> False
  Bottom -> False
  t2@(STypeVar _) -> t1 == t2
