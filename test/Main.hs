module Main (main) where
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (listDirectory, getCurrentDirectory)
import Data.Traversable (for)
import Data.Text.IO
import Prelude hiding (readFile)

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
  okTets <- for testCaseList $ \tc ->
    okTestGen tc (okTestsDir <> tc)
  pure $ testGroup "Ok" okTets

okTestGen :: FilePath -> FilePath -> IO TestTree
okTestGen testName filePath = do
  content <- readFile filePath
  pure $ testCase testName $
    content @?= content

badTestsGen :: IO TestTree
badTestsGen = do
  dir <- getCurrentDirectory
  let badTestsDir = dir <> "/test/stella-tests/bad/"
  testErrorsList <- listDirectory badTestsDir
  badTests <- for testErrorsList $ \err -> do
    let errDir = badTestsDir <> err <> "/"
    testCasesList <- listDirectory errDir
    tg <- for testCasesList $ \tc -> badTestGen tc err (errDir <> tc)
    pure $ testGroup err tg
  pure $ testGroup "Bad" badTests


badTestGen :: FilePath -> String -> FilePath -> IO TestTree
badTestGen testName _errorName filePath = do
  content <- readFile filePath
  pure $ testCase testName $
    content @?= content
