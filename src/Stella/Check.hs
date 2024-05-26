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
import Control.Monad.State (runStateT)
import Stella.Check.Constraints
import Stella.Check.Utils (pp)
import Data.Functor ((<&>))
import qualified Data.Text.IO as T
import qualified Data.Text as T


runStellaChecker :: Program -> IO (Either StellaError SType)
runStellaChecker program = do
  res <- run
  case res of
    Left err -> pure $ Left err
    Right ((resType, extensions), state) -> do
      if "#type-reconstruction" `elem` extensions
        then do
          T.putStrLn $ pp state.constraints
          case solve state.constraints of
            Left (InfinityType c) -> pure $ Left $ mkError c.expr ErrorOccursCheckInfiniteType
            Left (SolvingFailed c) -> pure $ Left $ mkError c.expr $ ErrorUnexpectedTypeForExpression c.lc c.rc
            Right solution -> do
              T.putStrLn "Solver result:"
              T.putStrLn . T.intercalate "\n" $ solution <&> \(t1, t2) -> pp t1 <> " = " <> pp t2
              pure $ Right resType
      else pure $ Right resType
  where
    run = runExceptT $ flip runStateT defState $ runReaderT (unCheckerM $ transProgram program) defEnv
