{-# LANGUAGE ViewPatterns #-}
module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (listDirectory, getCurrentDirectory)
import Data.Traversable (for)
import Data.Text.IO
import Prelude hiding (readFile)
import Stella.Check (StellaError, runStellaChecker, renderStellaError)
import Stella.Check.Types (SType)
import Stella.Ast.ParSyntax (myLexer, pProgram)
import Data.Either (isRight)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Either.Combinators (fromLeft')
import Data.Text (isInfixOf)

main :: IO ()
main = do
  tests <- testsGen
  defaultMain tests


testsGen :: IO TestTree
testsGen = do
  ok <- okTestsGen
  bad <- badTestsGen
  pure $ testGroup "Tests" [ok, bad]

okTestsGen :: IO TestTree
okTestsGen = do
  dir <- getCurrentDirectory
  let okTestsDir = dir <> "/test/stella-tests/ok/"
  testCaseList <- listDirectory okTestsDir
  (catMaybes -> okTets) <- for testCaseList $ \tc ->
    okTestGen tc (okTestsDir <> tc)
  pure $ testGroup "Ok" okTets

okTestGen :: FilePath -> FilePath -> IO (Maybe TestTree)
okTestGen testName filePath = do
  res <- runChecker filePath
  case res of
    Left err -> do
      print $ "Broken test, parser fail: " <> filePath <> "\nerror: " <> err
      pure Nothing
    Right checkerRes ->
      pure $ Just $ testCase testName $
        assertBool
          ("Should not fail with: " <> (T.unpack . renderStellaError . fromLeft' $ checkerRes))
          (isRight checkerRes)

badTestsGen :: IO TestTree
badTestsGen = do
  dir <- getCurrentDirectory
  let badTestsDir = dir <> "/test/stella-tests/bad/"
  testErrorsList <- listDirectory badTestsDir
  badTests <- for testErrorsList $ \err -> do
    let errDir = badTestsDir <> err <> "/"
    testCasesList <- listDirectory errDir
    (catMaybes -> tg) <- for testCasesList $ \tc -> badTestGen tc err (errDir <> tc)
    pure $ testGroup err tg
  pure $ testGroup "Bad" badTests


badTestGen :: FilePath -> String -> FilePath -> IO (Maybe TestTree)
badTestGen testName errorName filePath = do
  res <- runChecker filePath
  case res of
    Left err -> do
      print $ "Broken test, parser fail: " <> filePath <> "\nerror: " <> err
      pure Nothing
    Right checkerRes ->
      pure $ Just $ testCase testName $
        case checkerRes of
          Right _ -> assertFailure "Test should not suncceed"
          Left err -> do
            let msg = "Checker should finish with " <> errorName <> " \nBut finished with \n" <>  T.unpack (renderStellaError err)
            assertBool msg $
              T.pack errorName `isInfixOf` renderStellaError err

runChecker :: FilePath -> IO (Either String (Either StellaError SType))
runChecker path = do
  content <- readFile path
  let tokens = myLexer content
  case pProgram tokens of
    Left err -> pure $ Left err
    Right program -> Right <$> runStellaChecker program
