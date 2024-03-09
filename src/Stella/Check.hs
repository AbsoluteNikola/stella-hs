module Stella.Check
  ( module Trans
  , module Env
  , module Errors
  , module Types
  , runStellaChecker
  ) where

import Stella.Check.Trans as Trans
import Stella.Ast.AbsSyntax (Program)
import Stella.Check.Errors as Errors
import Stella.Check.Types as Types
import Stella.Check.Env as Env
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)

runStellaChecker :: Program -> IO (Either StellaError SType)
runStellaChecker program = runExceptT $ runReaderT (unCheckerM $ transProgram program) mempty
