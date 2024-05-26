module Main where
import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath, Semigroup ((<>))
  )
import Data.Text.IO   ( getContents, readFile )
import qualified Data.Text
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Stella.Ast.AbsSyntax   (Program)
import Stella.Ast.LexSyntax   ( Token, mkPosToken )
import Stella.Ast.ParSyntax   ( pProgram, myLexer )
import Stella.Ast.PrintSyntax ( printTree )
import Stella.Check
import Text.Pretty.Simple (pPrint)
import qualified Data.Text as T
import Stella.Check.Utils (pp)

type Err        = Either String
type ParseFun a = [Token] -> Err Program
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> Data.Text.Text -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: Int -> Program -> IO ()
showTree v tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n"
  -- pPrint tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
  putStrV v $ "\n[Type checker result]\n\n"
  typeCheckerRes <- runStellaChecker tree
  case typeCheckerRes of
    Right t -> putStrLn $ "Ok: " <> T.unpack (pp t)
    Left err -> do
      putStrLn $ T.unpack $ renderStellaError err
      exitFailure

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs
