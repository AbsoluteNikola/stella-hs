module Stella.Check.Env where

import Data.Map qualified as Map
import Stella.Check.Types (SType(..))
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import Stella.Check.Errors (StellaError)

data Env = Env
  { typesEnv :: Map.Map Text SType
  , termsEnv :: Map.Map Text SType
  } deriving (Show)

instance Semigroup Env where
  e1 <> e2 = Env
    { typesEnv = e1.typesEnv `Map.union` e2.typesEnv
    , termsEnv = e2.termsEnv `Map.union` e2.termsEnv
    }

instance Monoid Env where
  mempty = Env Map.empty Map.empty

newtype CheckerM a = CheckerM { unCheckerM :: ReaderT Env (ExceptT StellaError IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError StellaError, MonadIO)

withEnv :: Env -> CheckerM a -> CheckerM a
withEnv env = local (env <>)
