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

compileProgram :: Program -> FilePath -> IO ()
compileProgram program file = do
  let llvmCode = programToLLVM program
  putStrLn llvmCode
  codeToMonad llvmCode file

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
        Right _ -> hPutStrLn stderr "OK" >> compileProgram program file

codeToMonad :: String -> FilePath -> IO ()
codeToMonad code sourceFile = do
  putStr ("Compiling: " ++ sourceFile ++ "\n")
  let dir = takeDirectory sourceFile
  let baseName = takeBaseName sourceFile
  let llvmFile = replaceExtension sourceFile "ll"
  let unlinkedFile = dir </> ("unlinked_" ++ baseName) <.> "bc"
  let linkedFile = replaceExtension sourceFile "bc"
  let runtimeFile = "lib/runtime.bc"
  -- Save generated code to llvm file.
  writeFile llvmFile code
  -- Compile generated llvm file.
  let llvmProc1 = proc "llvm-as" ["-o", unlinkedFile, llvmFile]
  (_, _, _, llvmHandle1) <- createProcess llvmProc1
  waitForProcess llvmHandle1
  -- Link compiled file with runtime library
  let llvmProc2 = proc "llvm-link" ["-o", linkedFile, unlinkedFile, runtimeFile]
  (_, _, _, llvmHandle2) <- createProcess llvmProc2
  waitForProcess llvmHandle2
  -- Remove unlinked file
  let rmProc = proc "rm" [unlinkedFile]
  (_, _, _, rmHandle) <- createProcess rmProc
  waitForProcess rmHandle
  return ()

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
