module Stella.Check.Env where

import Data.Map qualified as Map
import Stella.Check.Types (SType(..))
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except
import Stella.Check.Errors (StellaError)
import Control.Applicative ((<|>))
import qualified Data.Set as Set

data Env = Env
  { typesEnv :: Map.Map Text SType
  , termsEnv :: Map.Map Text SType
  , exceptionType :: Maybe SType
  , extensions :: Set.Set Text
  } deriving (Show)

defEnv :: Env
defEnv = Env Map.empty Map.empty Nothing Set.empty

addTerms :: [(Text, SType)] -> Env -> Env
addTerms terms env = env{termsEnv = newTerms}
  where
    newTerms = Map.fromList terms `Map.union` env.termsEnv

concatEnv :: Env -> Env -> Env
concatEnv e1 e2 = Env
    { typesEnv = e2.typesEnv `Map.union` e1.typesEnv
    , termsEnv = e2.termsEnv `Map.union` e1.termsEnv
    , exceptionType = e1.exceptionType <|> e2.exceptionType
    , extensions = e1.extensions <> e2.extensions
    }

newtype CheckerM a = CheckerM { unCheckerM :: ReaderT Env (ExceptT StellaError IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError StellaError, MonadIO)

withEnv :: Env -> CheckerM a -> CheckerM a
withEnv env = local (`concatEnv` env)

withTerms :: [(Text, SType)] -> CheckerM a -> CheckerM a
withTerms terms = local (addTerms terms)

lookupTerm :: Text -> CheckerM (Maybe SType)
lookupTerm name = do
  env <- ask
  pure $ Map.lookup name env.termsEnv
