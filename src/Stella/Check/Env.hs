module Stella.Check.Env where

import Data.Map qualified as Map
import Stella.Check.Types (SType(..))
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import Stella.Check.Errors (StellaError)
import Control.Applicative ((<|>))
import qualified Data.Set as Set
import Control.Monad.State (StateT, MonadState(..), modify)
import Stella.Check.Constraints (Constraint (Constraint))
import Stella.Ast.AbsSyntax (Expr)

data Env = Env
  { typesEnv :: Map.Map Text SType
  , termsEnv :: Map.Map Text SType
  , universalTypeVars :: Set.Set Text
  , exceptionType :: Maybe SType
  , extensions :: Set.Set Text
  } deriving (Show)

data State = State
  { typeVarNum :: Int
  , constraints :: [Constraint]
  }

defState :: State
defState = State 0 []

defEnv :: Env
defEnv = Env
  { typesEnv = Map.empty
  , termsEnv = Map.empty
  , universalTypeVars = Set.empty
  , exceptionType = Nothing
  , extensions = Set.empty
  }

addTerms :: [(Text, SType)] -> Env -> Env
addTerms terms env = env{termsEnv = newTerms}
  where
    newTerms = Map.fromList terms `Map.union` env.termsEnv

addUniversalTypeVars :: [Text] -> Env -> Env
addUniversalTypeVars vars env = env
  { universalTypeVars = Set.fromList vars `Set.union` env.universalTypeVars
  }

concatEnv :: Env -> Env -> Env
concatEnv e1 e2 = Env
    { typesEnv = e2.typesEnv `Map.union` e1.typesEnv
    , termsEnv = e2.termsEnv `Map.union` e1.termsEnv
    , universalTypeVars = e1.universalTypeVars <> e2.universalTypeVars
    , exceptionType = e1.exceptionType <|> e2.exceptionType
    , extensions = e1.extensions <> e2.extensions
    }

newtype CheckerM a = CheckerM { unCheckerM :: ReaderT Env (StateT State (ExceptT StellaError IO)) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError StellaError, MonadIO, MonadState State)

withEnv :: Env -> CheckerM a -> CheckerM a
withEnv env = local (`concatEnv` env)

withTerms :: [(Text, SType)] -> CheckerM a -> CheckerM a
withTerms terms = local (addTerms terms)

withUniversalVars :: [Text] -> CheckerM a -> CheckerM a
withUniversalVars vars = local (addUniversalTypeVars vars)

lookupTerm :: Text -> CheckerM (Maybe SType)
lookupTerm name = do
  env <- ask
  pure $ Map.lookup name env.termsEnv

lookupUniversalVar :: Text -> CheckerM Bool
lookupUniversalVar name = do
  env <- ask
  pure $ name `Set.member` env.universalTypeVars


newTypeVar :: CheckerM SType
newTypeVar = do
  (typeVarNum -> curIndex) <- get
  modify $ \s -> s{typeVarNum = curIndex + 1}
  pure $ STypeVar curIndex

isSubtypingEnabled :: CheckerM Bool
isSubtypingEnabled = asks (Set.member "#structural-subtyping" . extensions)

isAmbiguousTypesAsBottomEnabled :: CheckerM Bool
isAmbiguousTypesAsBottomEnabled = asks (Set.member "#ambiguous-type-as-bottom" . extensions)

isTypeReconstructionEnabled :: CheckerM Bool
isTypeReconstructionEnabled = asks (Set.member "#type-reconstruction" . extensions)

isUniversalTypesEnabled :: CheckerM Bool
isUniversalTypesEnabled = asks (Set.member "#universal-types" . extensions)

addConstraint :: SType -> SType -> Expr -> CheckerM ()
addConstraint t1 t2 expr = do
  let c = Constraint t1 t2 expr
  modify $ \s -> s{constraints = c : s.constraints}
