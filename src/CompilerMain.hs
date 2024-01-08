module Main where

import System.Environment ( getArgs )
import System.IO (hPutStrLn, stderr)
import System.Process
import System.FilePath
import System.Exit (exitFailure)

import qualified Data.Maybe

import AbsLatte
import ParLatte
import TypeChecker
import Compiler

compileProgram :: Program -> IO ()
compileProgram program = do
  let llvmCode = programToLLVM program
  putStrLn llvmCode

checkFile :: String -> IO ()
checkFile file = do
  fileContent <- readFile file
  let tokens = myLexer fileContent
  let maybeProgram = pProgram tokens
  case maybeProgram of
    Left parser_err -> hPutStrLn stderr "ERROR" >> putStrLn ("Err from file " ++ file ++ ": " ++ parser_err) >> exitFailure
    Right program -> do
      let typeCheckResult = executeProgramCheck program
      case typeCheckResult of
        Left typeCheckErr -> hPutStrLn stderr "ERROR" >> putStrLn ("Err from file " ++ file ++ ": " ++ typeCheckErr) >> exitFailure
        Right _ -> hPutStrLn stderr "OK" >> compileProgram program

usage :: IO ()
usage = do
  (_, _, _,handle) <- createProcess (shell "ls")
  waitForProcess handle
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Compile content of each file."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs         -> mapM_ checkFile fs
