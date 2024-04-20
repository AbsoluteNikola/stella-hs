-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

module Stella.Check.Trans where

import Prelude
import Stella.Ast.AbsSyntax
import Stella.Check.Types
import Data.Text (Text)
import Data.Foldable
import qualified Stella.Check.Env as Env
import Stella.Check.Env (CheckerM)
import qualified Data.Map as Map
import Control.Monad.Except ( MonadError(throwError))
import Stella.Check.Errors (mkError, ErrorType (..))
import Control.Monad.IO.Class (liftIO)
import Text.Pretty.Simple (pPrint)
import Data.Functor ((<&>))
import Control.Monad (when, join, unless, foldM)
import Stella.Ast.PrintSyntax (Print)
import Control.Monad.Reader (ask)
import qualified Data.List as L
import Control.Applicative (liftA2)
import qualified Data.List.NonEmpty as NE
import Data.Traversable (for)
import Stella.Check.Utils (Pretty(pp))
import Stella.Check.Exhaustiveness (checkPatternsExhaustive)
import qualified Data.Set as Set
import Control.Monad.Reader.Class (asks)

type Checker = CheckerM SType

failNotImplemented :: forall a b. (HasPosition a, Print a) => a -> CheckerM b
failNotImplemented node = throwError $ mkError node ErrorUnimplementedCase

failWith :: forall a b. (HasPosition a, Print a) => a -> ErrorType -> CheckerM b
failWith x err = throwError $ mkError x err

debugPrint :: Show a => a ->  CheckerM ()
debugPrint = liftIO . pPrint

debugPrintEnv :: CheckerM ()
debugPrintEnv = do
  env <- ask
  liftIO . pPrint $ env

whenTypeNotEq :: SType -> SType -> CheckerM () -> CheckerM ()
whenTypeNotEq actual expected action = do
  isSubtypingEnabled
    <- asks (Set.member "#structural-subtyping" . Env.extensions)
  let
    eqF = if isSubtypingEnabled
        then eqWithSubtyping -- (==)
        else eqWithSubtyping
  when (not $ eqF actual expected) $ action

transProgram :: Program -> Checker
transProgram x = case x of
  AProgram pos languagedecl extensions decls -> do
    env <- transDeclSignatures decls
    let
      extensionsList = join $ extensions
        <&> \(AnExtension _ exts) ->
          exts <&> \(ExtensionName n) -> n
      envWithExtensions = env{Env.extensions = Set.fromList extensionsList}
    Env.withEnv envWithExtensions $
      for_ decls transDecl
    mainFunction <-  case Map.lookup "main" env.termsEnv of
      Just (FuncType ftd)
        | length ftd.argsType == 1 -> pure ftd
        | otherwise -> failWith x (ErrorIncorrectArityOfMain $ length ftd.argsType)
      _ -> failWith x ErrorMissingMain
    pure mainFunction.returnType

data TransDeclData = TransDeclData
  { typeAliases :: [(Text, SType)]
  , functions :: [(Text, (FuncTypeData, Decl))]
  , exceptionType :: Maybe SType
  } deriving (Show)

transDeclSignatures :: [Decl] -> CheckerM Env.Env
transDeclSignatures decls = do
  transData <- transDeclsTypes decls
  pure Env.Env
    { typesEnv = Map.fromList transData.typeAliases
    , termsEnv = Map.fromList $ transData.functions
      <&> \(name, (ftd, _)) -> (name, FuncType ftd)
    , exceptionType = transData.exceptionType
    , extensions = mempty
    }

transDeclsTypes :: [Decl] -> CheckerM TransDeclData
transDeclsTypes = foldlM go start
  where
    go :: TransDeclData -> Decl -> CheckerM TransDeclData
    go cur dec = case dec of
      DeclFun pos annotations (StellaIdent name) paramdecls returntype throwtype decls expr -> do
        argsTypes <- fmap snd <$> traverse transParamDecl paramdecls
        returnType <- transReturnType returntype
        let
          func = FuncTypeData
            { argsType = argsTypes
            , returnType = returnType
            }
        pure $ cur{functions = (name, (func, dec)) : cur.functions}
      DeclTypeAlias pos (StellaIdent name) type_ -> do
        t <- transType type_
        pure $ cur{typeAliases = (name, t) : cur.typeAliases}
      DeclExceptionType pos t -> do
        exceptionType <- transType t
        pure $ cur{exceptionType = Just exceptionType}
      DeclExceptionVariant pos (StellaIdent name) t -> do
        exceptionType <- transType t
        case cur.exceptionType of
          Just (VariantType vtd) ->
            let newExceptionType = Just . VariantType . VariantTypeData
                  $ Map.insert name (Just exceptionType) vtd.variants
            in pure $ cur{exceptionType = newExceptionType}
          _ ->
            let newExceptionType = VariantType VariantTypeData
                  { variants = Map.singleton name (Just exceptionType) }
            in pure cur{exceptionType = Just newExceptionType}
      _ -> failNotImplemented dec
    start = TransDeclData [] [] Nothing

transLanguageDecl :: LanguageDecl -> Checker
transLanguageDecl x = case x of
  LanguageCore pos -> failNotImplemented x

transExtension :: Extension -> Checker
transExtension x = case x of
  AnExtension pos extensionnames -> failNotImplemented x

transDecl :: Decl -> CheckerM (Maybe SType)
transDecl x = case x of
  DeclFun pos annotations (StellaIdent name) paramdecls returntype throwtype decls expr -> do
    env <- transDeclSignatures decls
    paramsTypes <- traverse transParamDecl paramdecls
    retType <- transReturnType returntype
    let envWithParams = Env.addTerms paramsTypes env
    exprT <- Env.withEnv envWithParams $ do
      traverse_ transDecl decls
      transExpr (Just retType) expr
    whenTypeNotEq retType exprT $
      failWith expr (ErrorUnexpectedTypeForExpression exprT retType)
    pure $ Just exprT
  DeclFunGeneric pos annotations (StellaIdent name) stellaidents paramdecls returntype throwtype decl expr -> failNotImplemented x
  DeclTypeAlias pos (StellaIdent name) type_ -> Just <$> transType type_
  DeclExceptionType pos type_ -> pure Nothing
  DeclExceptionVariant pos (StellaIdent name) type_ -> pure Nothing

-- looks like dead code
transLocalDecl :: LocalDecl -> Checker
transLocalDecl x = case x of
  ALocalDecl pos decl -> failNotImplemented x

transAnnotation :: Annotation -> Checker
transAnnotation x = case x of
  InlineAnnotation pos -> failNotImplemented x

transParamDecl :: ParamDecl -> CheckerM (Text, SType)
transParamDecl x = case x of
  AParamDecl pos (StellaIdent name) type_ -> (name,) <$> transType type_

transReturnType :: ReturnType -> Checker
transReturnType x = case x of
  NoReturnType pos -> pure $ SimpleType Unit
  SomeReturnType pos type_ -> transType type_

transThrowType :: ThrowType -> Checker
transThrowType x = case x of
  NoThrowType pos -> failNotImplemented x
  SomeThrowType pos types -> failNotImplemented x

transType :: Type -> Checker
transType x = case x of
  TypeFun pos types retType -> do
    argsTypes <- traverse transType types
    returnType <- transType retType
    pure $ FuncType FuncTypeData
      { argsType = argsTypes
      , returnType = returnType
      }
  TypeForAll pos stellaidents type_ -> failNotImplemented x
  TypeRec pos (StellaIdent name) type_ -> failNotImplemented x
  TypeSum pos type_1 type_2 -> do
    t1 <- transType type_1
    t2 <- transType type_2
    pure $ SumType SumTypeData
      { leftType = t1
      , rightType = t2
      }
  TypeTuple pos types' -> do
    types <- traverse transType types'
    pure $ TupleType TupleTypeData
      { tupleTypes = types
      }
  TypeRecord pos recordfieldtypes -> do
    recordFields <- traverse transRecordFieldType recordfieldtypes
    checkThatNamesUniq x ErrorDuplicateRecordFields$ fst <$> recordFields
    pure $ RecordType RecordTypeData
      { recordFields = recordFields
      }
  TypeVariant pos variantfieldtypes -> do
    variantFields <- traverse transVariantFieldType variantfieldtypes
    checkThatNamesUniq x ErrorDuplicateRecordFields $ fst <$> variantFields
    pure $ VariantType VariantTypeData
      { variants = Map.fromList variantFields
      }
  TypeList pos type_ -> do
    innerType <- transType type_
    pure $ ListType innerType
  TypeBool pos -> pure $ SimpleType Boolean
  TypeNat  pos-> pure $ SimpleType Nat
  TypeUnit pos -> pure $ SimpleType Unit
  TypeTop pos -> pure Top
  TypeBottom pos -> pure Bottom
  TypeRef pos type_ -> do
    innerType <- transType type_
    pure $ RefType innerType
  TypeVar pos (StellaIdent name) -> pure $ TypeVarType name
  TypeAuto{} -> failNotImplemented x

transMatchCase :: Maybe SType -> SType -> MatchCase -> Checker
transMatchCase desiredType matchType x = case x of
  AMatchCase pos pattern_ expr -> do
    newVars <- transPattern matchType pattern_
    exprT <- Env.withTerms newVars $
      transExpr desiredType expr
    pure exprT

transOptionalTyping :: OptionalTyping -> Checker
transOptionalTyping x = case x of
  NoTyping  pos-> failNotImplemented x
  SomeTyping  pos type_ -> failNotImplemented x

transPatternData :: PatternData -> Checker
transPatternData x = case x of
  NoPatternData pos-> failNotImplemented x
  SomePatternData pos pattern_ -> failNotImplemented x

transExprData :: Maybe SType -> ExprData -> CheckerM (Maybe SType)
transExprData desiredType x = case x of
  NoExprData pos -> pure Nothing
  SomeExprData pos expr -> Just <$> transExpr desiredType expr

transPattern :: SType -> Pattern -> CheckerM [(Text, SType)]
transPattern t x = case x of
  PatternVariant pos (StellaIdent name) patterndata -> case t of
    VariantType (VariantTypeData vtd) -> case Map.lookup name vtd of
      -- more nested cases for god of nested cases
      Just variantType -> case (variantType, patterndata) of
        (Nothing, NoPatternData _) -> pure []
        (Just variantCaseType, SomePatternData pos' pattern) ->
          transPattern variantCaseType pattern
        (Nothing, SomePatternData{}) ->
          failWith x ErrorUnexpectedNonNullaryVariantPattern
        (Just _, NoPatternData _) ->
          failWith x ErrorUnexpectedNullaryVariantPattern
      Nothing -> failWith x ErrorUnexpectedPatternForType
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternInl pos pattern_ -> case t of
    SumType std -> transPattern std.leftType pattern_
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternInr pos pattern_ -> case t of
    SumType std -> transPattern std.rightType pattern_
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternTuple pos patterns -> case t of
    TupleType (TupleTypeData tupleTypes)
      | length tupleTypes == length patterns
      -> do
        (join -> newVars) <- for (zip tupleTypes patterns) $
          uncurry transPattern
        checkThatNamesUniq x ErrorDuplicatePatternVariable (fmap fst newVars)
        pure newVars
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternRecord pos labelledpatterns -> case t of
    RecordType rtd -> do
      (join -> newVars) <- for labelledpatterns $ transLabelledPattern rtd
      checkThatNamesUniq x ErrorDuplicatePatternVariable (fmap fst newVars)
      pure newVars
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternList pos patterns -> case t of
    ListType listInnerType -> do
      (join -> newVars) <- for patterns $ transPattern listInnerType
      checkThatNamesUniq x ErrorDuplicatePatternVariable (fmap fst newVars)
      pure newVars
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternCons pos pattern_1 pattern_2 -> case t of
    ListType listInnerType -> do
      newVars1 <- transPattern listInnerType pattern_1
      newVars2 <- transPattern (ListType listInnerType) pattern_2
      let newVars = newVars1 ++ newVars2
      checkThatNamesUniq x ErrorDuplicatePatternVariable (fmap fst newVars)
      pure newVars
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternFalse pos -> case t of
    SimpleType Boolean -> pure []
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternTrue pos -> case t of
    SimpleType Boolean -> pure []
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternUnit pos -> case t of
    SimpleType Unit -> pure []
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternInt pos integer -> case t of
    SimpleType Nat -> pure []
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternSucc pos pattern_ -> case t of
    SimpleType Nat -> transPattern (SimpleType Nat) pattern_
    _ -> failWith x ErrorUnexpectedPatternForType
  PatternVar pos (StellaIdent name) -> pure [(name, t)]
  PatternAsc pos pattern type_ -> do
    patT <- transType type_
    whenTypeNotEq patT t $
      failWith x ErrorUnexpectedPatternForType
    transPattern patT pattern
  PatternCastAs {} -> failNotImplemented x

transLabelledPattern :: RecordTypeData -> LabelledPattern -> CheckerM [(Text, SType)]
transLabelledPattern (RecordTypeData rtd) x = case x of
  ALabelledPattern pos (StellaIdent name) pattern_
    | Just fieldType <- lookup name rtd
    -> transPattern fieldType pattern_
  _ -> failWith x ErrorUnexpectedPatternForType

transExpr :: Maybe SType -> Expr -> Checker
transExpr desiredType x = case x of
  Sequence pos expr1 expr2 -> do
    expr1Type <- transExpr (Just unit_) expr1
    whenTypeNotEq expr1Type unit_ $
      failWith expr1 (ErrorUnexpectedTypeForExpression unit_ expr1Type)
    expr2Type <- transExpr desiredType expr2
    pure expr2Type
  Assign pos expr1 expr2 -> do
    expr1Type <- transExpr Nothing expr1
    t <- case expr1Type of
      RefType innerType -> pure innerType
      _ -> failWith x ErrorNotAReference
    expr2Type <- transExpr (Just t) expr2
    whenTypeNotEq t expr2Type $
      failWith expr1 $ ErrorUnexpectedTypeForExpression t expr2Type
    pure unit_
  If pos expr1 expr2 expr3 -> do
    expr1Type <- transExpr (Just bool_) expr1
    whenTypeNotEq expr1Type bool_ $
      failWith expr1 $ ErrorUnexpectedTypeForExpression expr1Type bool_
    expr2Type <- transExpr desiredType expr2
    expr3Type <- transExpr (Just expr2Type) expr3
    whenTypeNotEq expr2Type expr3Type $
      failWith expr3 $ ErrorUnexpectedTypeForExpression expr2Type expr3Type
    pure expr2Type
  Let pos patternbindings expr -> do
    let
      travBindings :: [(Text, SType)] -> PatternBinding -> CheckerM [(Text, SType)]
      travBindings newVars binding = Env.withTerms newVars $ do
        newVarsFromPattern <- transPatternBinding binding
        pure $ newVars ++ newVarsFromPattern
    newVars <- foldM travBindings [] patternbindings
    exprT <- Env.withTerms newVars $ do
      transExpr desiredType expr
    pure exprT
  LetRec pos patternbindings expr -> do
    let
      travBindings :: [(Text, SType)] -> PatternBinding -> CheckerM [(Text, SType)]
      travBindings newVars binding = Env.withTerms newVars $ do
        newVarsFromPattern <- transLetRecPatternBinding binding
        pure $ newVars ++ newVarsFromPattern
    newVars <- foldM travBindings [] patternbindings
    exprT <- Env.withTerms newVars $ do
      transExpr desiredType expr
    pure exprT
  TypeAbstraction pos stellaidents expr -> failNotImplemented x
  LessThan pos expr1 expr2 -> compareNatsOperator expr1 expr2
  LessThanOrEqual pos expr1 expr2 -> compareNatsOperator expr1 expr2
  GreaterThan pos expr1 expr2 -> compareNatsOperator expr1 expr2
  GreaterThanOrEqual pos expr1 expr2 -> compareNatsOperator expr1 expr2
  Equal pos expr1 expr2 -> compareNatsOperator expr1 expr2
  NotEqual pos expr1 expr2 -> compareNatsOperator expr1 expr2
  TypeAsc pos expr type_ -> do
    t <- transType type_
    exprT <- transExpr (Just t) expr
    whenTypeNotEq exprT t $
      failWith expr $ ErrorUnexpectedTypeForExpression exprT t
    pure exprT
  TypeCast pos expr type_ -> failNotImplemented x
  Abstraction pos paramdecls expr -> do
    mDesiredRetType <- case desiredType of
      Nothing -> pure Nothing
      Just (FuncType ftd) -> pure $ Just ftd.returnType
      Just _ -> pure Nothing
    paramsTypes <- traverse transParamDecl paramdecls
    retType <- Env.withTerms paramsTypes $ transExpr mDesiredRetType expr
    let
      funcT = FuncType FuncTypeData
        { argsType = snd <$> paramsTypes
        , returnType = retType
        }
    pure funcT
  Variant pos (StellaIdent name) exprdata -> case desiredType of
    Just v@(VariantType (VariantTypeData vtd)) -> case Map.lookup name vtd of
      Just variantType -> do
        exprT <- transExprData variantType exprdata
        case (exprT, variantType) of
          (Nothing, Nothing) -> pure ()
          (Just et, Just vt)
            | et == vt -> pure ()
            | otherwise -> failWith x $ ErrorUnexpectedTypeForExpression et vt
          (Nothing, Just vt ) -> failWith x $ ErrorMissingDataForLabel name
          (Just et, Nothing) -> failWith x $ ErrorUnexpectedDataForNullaryLabel et
        pure v
      Nothing -> failWith x (ErrorUnexpectedVariantLabel name)
    Just dt -> failWith x $ ErrorUnexpectedVariant (Just dt)
    Nothing -> failWith x ErrorAmbiguousVariantType
  Match pos expr matchcases -> do
    exprT <- transExpr Nothing expr
    casesT <- case NE.nonEmpty matchcases of
      Nothing -> failWith x ErrorIllegalEmptyMatching
      Just cases -> do
        firstCaseExprT <- transMatchCase desiredType exprT (NE.head cases)
        for_ (NE.tail cases) $ \c -> do
          ct <- transMatchCase desiredType exprT c
          whenTypeNotEq ct firstCaseExprT $
            failWith expr (ErrorUnexpectedTypeForExpression ct firstCaseExprT)
        let patterns = cases <&> \(AMatchCase _ pattern _) -> pattern
        unless (checkPatternsExhaustive exprT patterns) $
          failWith x ErrorNonExhaustiveMatchPattern
        pure firstCaseExprT
    pure casesT
  Add pos expr1 expr2 -> arithmeticNatsOperator expr1 expr2
  Subtract pos expr1 expr2 -> arithmeticNatsOperator expr1 expr2
  LogicOr pos expr1 expr2 -> logicOperator expr1 expr2
  Multiply pos expr1 expr2 -> arithmeticNatsOperator expr1 expr2
  Divide pos expr1 expr2 -> arithmeticNatsOperator expr1 expr2
  LogicAnd pos expr1 expr2 -> logicOperator expr1 expr2
  Ref pos expr -> do
    t <- transExpr Nothing expr -- TODO: pass desired type
    pure $ RefType t
  Deref pos expr -> do
    t <- transExpr Nothing expr -- TODO: pass desired type
    case t of
      RefType innerType -> pure innerType
      _ -> failWith x ErrorNotAReference
  Application pos func args -> do
    funcType <- transExpr Nothing func >>= \case
      FuncType ftd
        | length ftd.argsType /= length args
        -> failWith x (ErrorIncorrectNumberOfArguments $ length args)
        | otherwise -> pure ftd
      _ -> failWith func ErrorNotAFunction
    argsWithTypes <-
      for (zip args funcType.argsType) $ \(expr, t) ->
        (expr,) <$> transExpr (Just t) expr
    -- some duplication of type check logic. TODO: rewrite in better times
    checkFunctionApplication x funcType argsWithTypes
  TypeApplication pos expr types -> failNotImplemented x
  DotRecord pos expr (StellaIdent name) -> do
    rtd <- transExpr Nothing expr >>= \case
      RecordType (RecordTypeData rtd) -> pure rtd
      _ -> failWith expr ErrorNotARecord
    case lookup name rtd of
      Just t -> pure t
      Nothing -> failWith expr (ErrorUnexpectedFieldAccess name)
  DotTuple pos expr index -> do
    ttd <- transExpr Nothing expr >>= \case
      TupleType (TupleTypeData ttd) -> pure ttd
      _ -> failWith expr ErrorNotATuple
    case ttd !!? (index - 1) of
      Just t -> pure t
      Nothing -> failWith expr (ErrorTupleIndexOutOfBounds index)
  Tuple pos exprs -> do
    types <- traverse (transExpr Nothing) exprs
    pure $ TupleType TupleTypeData
      { tupleTypes = types
      }
  Record pos bindings_ -> do
    let
      desiredFields = case desiredType of
        Just (RecordType (RecordTypeData rtd)) -> Just rtd
        _ -> Nothing
    bindings <- for bindings_ $ \(ABinding _ (StellaIdent name) expr) -> do
      let
        desiredTypeForField = lookup name =<< desiredFields
      type_ <- transExpr desiredTypeForField expr
      pure (name, type_)
    checkThatNamesUniq x ErrorDuplicateRecordFields $ fst <$> bindings
    pure $ RecordType RecordTypeData
      { recordFields = bindings
      }
  ConsList pos expr1 expr2 -> do
    expr1T <- transExpr Nothing expr1
    expr2T <- transExpr (Just $ ListType expr1T) expr2
    whenTypeNotEq (ListType expr1T) expr2T $
      failWith expr1 $ ErrorUnexpectedTypeForExpression (ListType expr1T) expr2T
    pure $ ListType expr1T
  Head pos expr -> do
    listInnerType <- transExpr Nothing expr >>= \case
      ListType listInnerType -> pure listInnerType
      _ -> failWith expr ErrorNotAList
    pure listInnerType
  IsEmpty pos expr -> do
    transExpr Nothing expr >>= \case
      ListType _ -> pure ()
      _ -> failWith expr ErrorNotAList
    pure bool_
  Tail pos expr -> do
    listType <- transExpr Nothing expr >>= \case
      lt@(ListType _) -> pure lt
      _ -> failWith expr ErrorNotAList
    pure listType
  List pos exprs -> do
    case exprs of
      -- empty list, should infer type
      [] -> case desiredType of
        Just (ListType t) -> pure (ListType t)
        Just dt -> failWith x $ ErrorUnexpectedTypeForExpressionText $ "Expected " <> pp dt <> "but got list"
        Nothing -> failWith x ErrorAmbiguousList
      (e:es) -> do
        -- with some manipulations we can understand desired type
        eT <- transExpr Nothing e
        for_ es $ \e' -> do
          e'T <- transExpr Nothing e'
          whenTypeNotEq e'T eT $
            failWith e' $ ErrorUnexpectedTypeForExpression e'T eT
        pure $ ListType eT
  Panic pos -> case desiredType of
    Just t -> pure t
    Nothing -> failWith x ErrorAmbiguousPanicType
  Throw pos expr -> do
    env <- ask
    case env.exceptionType of
      Nothing -> failWith x ErrorExceptionTypeNotDeclared
      Just excT -> do
        exprT <- transExpr (Just excT) expr
        whenTypeNotEq exprT excT $
          failWith expr $ ErrorUnexpectedTypeForExpression exprT excT
    case desiredType of
      Nothing ->  failWith x ErrorAmbiguousThrowType
      Just t -> pure t
  TryCatch pos expr1 pattern_ expr2 -> do
    exprT <- transExpr desiredType expr1
    env <- ask
    excT <- case env.exceptionType of
      Nothing -> failWith x ErrorExceptionTypeNotDeclared
      Just t -> pure t
    newTerms <- transPattern excT pattern_
    catchT <- Env.withTerms newTerms $ do
      transExpr desiredType expr2
    whenTypeNotEq exprT catchT $
      failWith x $ ErrorUnexpectedTypeForExpression exprT catchT
    pure $ exprT
  TryWith pos expr1 expr2 -> do
    t1 <- transExpr desiredType expr1
    t2 <- transExpr desiredType expr2
    whenTypeNotEq t1 t2 $
      failWith expr2 $ ErrorUnexpectedTypeForExpression t1 t2
    pure t1
  TryCastAs{} -> failNotImplemented x
  Inl pos expr -> case desiredType of
    Just (SumType std) -> do
      exprT <- transExpr (Just std.leftType) expr
      whenTypeNotEq exprT std.leftType $
        failWith expr $ ErrorUnexpectedTypeForExpression exprT std.leftType
      pure $ SumType std
    Just dt -> failWith x ErrorUnexpectedInjection
    Nothing -> failWith x ErrorAmbiguousSumType
  Inr pos expr -> case desiredType of
    Just (SumType std) -> do
      exprT <- transExpr (Just std.rightType) expr
      whenTypeNotEq exprT std.rightType $
          failWith expr $ ErrorUnexpectedTypeForExpression exprT std.rightType
      pure $ SumType std
    Just dt -> failWith x ErrorUnexpectedInjection
    Nothing -> failWith x ErrorAmbiguousSumType
  Succ pos expr -> do
    exprT <- transExpr (Just nat_) expr
    whenTypeNotEq exprT nat_ $
      failWith expr $ ErrorUnexpectedTypeForExpression exprT nat_
    pure nat_
  LogicNot pos expr -> do
    exprT <- transExpr (Just bool_) expr
    whenTypeNotEq exprT bool_ $
      failWith expr $ ErrorUnexpectedTypeForExpression exprT bool_
    pure nat_
  Pred pos expr ->  do
    exprT <- transExpr (Just nat_) expr
    whenTypeNotEq exprT nat_ $
      failWith expr $ ErrorUnexpectedTypeForExpression exprT nat_
    pure nat_
  IsZero pos expr -> do
    exprT <- transExpr (Just nat_) expr
    whenTypeNotEq exprT nat_ $
      failWith expr $ ErrorUnexpectedTypeForExpression exprT nat_
    pure bool_
  Fix pos expr -> do
    let
      desiredTypeForFix = desiredType <&> \t ->
        FuncType FuncTypeData{argsType = [t], returnType = t}
    ftd <- transExpr desiredTypeForFix expr >>= \case
      FuncType ftd
        | length ftd.argsType == 1 -> pure ftd
        | otherwise -> failWith expr $ ErrorUnexpectedTypeForExpressionText $ "Expected function with one argument, but got" <> pp ftd
      _ -> failWith expr ErrorNotAFunction
    pure ftd.returnType
  NatRec pos expr1 expr2 expr3 -> do
    untilT <- transExpr (Just nat_) expr1
    whenTypeNotEq untilT nat_ $
      failWith expr1 $ ErrorUnexpectedTypeForExpression untilT nat_
    startT <- transExpr desiredType expr2
    let
      masterFunctionT = FuncType $ FuncTypeData
        { argsType = [nat_]
        , returnType = FuncType $ FuncTypeData
          { argsType = [startT]
          , returnType = startT
          }
        }
    funcT <- transExpr desiredType expr3
    whenTypeNotEq funcT masterFunctionT $
      failWith expr3 $ ErrorUnexpectedTypeForExpression funcT masterFunctionT
    pure startT
  Fold pos type_ expr -> failNotImplemented x
  Unfold pos type_ expr -> failNotImplemented x
  ConstTrue pos -> pure bool_
  ConstFalse pos -> pure bool_
  ConstUnit pos -> pure unit_
  ConstInt pos integer -> pure nat_
  ConstMemory pos memoryaddress -> failNotImplemented x -- fixme
  Var pos (StellaIdent name) -> lookupTermThrow x name

transPatternBinding :: PatternBinding -> CheckerM [(Text, SType)]
transPatternBinding x = case x of
  APatternBinding pos pattern_ expr -> do
    exprT <- transExpr Nothing expr
    newTerms <- transPattern exprT pattern_
    pure newTerms

transLetRecPatternBinding :: PatternBinding -> CheckerM [(Text, SType)]
transLetRecPatternBinding x = case x of
  APatternBinding pos (PatternAsc _ (PatternVar _ (StellaIdent name)) type_) expr -> do
    asT <- transType type_
    exprT <- Env.withTerms [(name, asT)] $ transExpr (Just asT) expr
    whenTypeNotEq asT exprT $
      failWith x $ ErrorUnexpectedTypeForExpression asT exprT
    pure [(name, asT)]
  _ -> failWith x $ ErrorAmbiguousPatternType "Let rec supported only with 'letrec x as T = x' pattern"

transVariantFieldType :: VariantFieldType -> CheckerM (Text, Maybe SType)
transVariantFieldType x = case x of
  AVariantFieldType pos (StellaIdent name) (SomeTyping typingPos type_) -> do
    t <- transType type_
    pure (name, Just t)
  AVariantFieldType pos (StellaIdent name) (NoTyping typingPos) -> do
    pure (name, Nothing)

transRecordFieldType :: RecordFieldType -> CheckerM (Text, SType)
transRecordFieldType x = case x of
  ARecordFieldType pos (StellaIdent name) type_ -> do
    t <- transType type_
    pure (name, t)

-- dead code?
transTyping :: Typing -> Checker
transTyping x = case x of
  ATyping pos expr type_ -> failNotImplemented x

compareNatsOperator :: Expr -> Expr -> CheckerM SType
compareNatsOperator expr1 expr2 =  do
  expr1Type <- transExpr (Just nat_) expr1
  whenTypeNotEq expr1Type nat_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr1Type nat_
  expr2Type <- transExpr (Just nat_) expr1
  whenTypeNotEq expr2Type nat_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr2Type nat_
  pure bool_

arithmeticNatsOperator :: Expr -> Expr -> CheckerM SType
arithmeticNatsOperator expr1 expr2 =  do
  expr1Type <- transExpr (Just nat_) expr1
  whenTypeNotEq expr1Type nat_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr1Type nat_
  expr2Type <- transExpr (Just nat_) expr1
  whenTypeNotEq expr2Type nat_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr2Type nat_
  pure nat_

logicOperator :: Expr -> Expr -> CheckerM SType
logicOperator expr1 expr2 =  do
  expr1Type <- transExpr (Just bool_) expr1
  whenTypeNotEq expr1Type bool_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr1Type bool_
  expr2Type <- transExpr (Just bool_) expr1
  whenTypeNotEq expr2Type bool_ $
    failWith expr1 $ ErrorUnexpectedTypeForExpression expr1Type bool_
  pure bool_

checkFunctionApplication :: {- Application expr -} Expr -> FuncTypeData -> [(Expr, SType)] -> CheckerM SType
checkFunctionApplication applicationExpr ftd passed = do
    go ftd.argsType passed
    pure ftd.returnType
  where
    go :: {-functions args-} [SType] -> {-passed args-} [(Expr, SType)] -> CheckerM ()
    go (at:ats) ((expr, pa):pas) =
      if at /= pa
        then failWith expr $ ErrorUnexpectedTypeForExpression at pa
        else go ats pas
    go [] [] = pure ()
    go _ _ = failWith applicationExpr ErrorUnexpectedTypeForAParameter

checkThatNamesUniq :: (HasPosition node, Print node) => node -> ([Text] -> ErrorType) -> [Text] -> CheckerM ()
checkThatNamesUniq expr errorConstructor names = case filter ((> 1) . snd) counts of
    [] -> pure ()
    (fmap fst -> nonUniqNames) -> failWith expr (errorConstructor nonUniqNames)
  where
    counts = map (liftA2 (,) head length) . L.group . L.sort $ names

lookupTermThrow :: (HasPosition node, Print node) => node -> Text -> CheckerM SType
lookupTermThrow node name = Env.lookupTerm name >>= \case
  Just type_ -> pure type_
  Nothing -> failWith node ErrorUndefinedVariable

-- Thanks relude :)
infix 9 !!?
(!!?) :: [a] -> Integer -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Integer -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!!?) #-}
