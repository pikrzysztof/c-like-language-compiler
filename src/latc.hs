-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import Gramatyka.LexLatte
import Gramatyka.ParLatte

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.Reader
import Gramatyka.PrintLatte
import Gramatyka.AbsLatte

import CompilerEnv as CE
import CompilerState as CS
import Checker
import Compiler



import Gramatyka.ErrM

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = -- putStrLn f >>
  readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = do
  let ts = myLLexer s
  case p ts of
    Bad str  -> do
      putStrLn "ERROR\nParse              Failed..."
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn str
      exitFailure
    Ok tree -> do
--      showTree v tree
      let checked = runExcept $ checkTree tree
      case checked of
        Left err -> do
          putStrLn (show err)
          exitFailure
        Right newTree -> do
          let x = runReader (execStateT (compile newTree) CS.stateZero) CE.envZero
          putStrLn (showCompiled x)
          showTree v newTree
          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hGetContents stdin >>= run 2 pProgram
    "-v":fs -> mapM_ (runFile 2 (pProgram :: [Token] -> Err Program)) fs
    fs -> mapM_ (runFile 0 pProgram) fs
