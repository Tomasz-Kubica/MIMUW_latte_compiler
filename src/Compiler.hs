module Compiler where

import qualified Data.Map

import AbsLatte
import QuadrupleCode
import LatteStdLibFunctions
import QuadrupleGeneration
import QuadrupleCodeToBlocks
import BlocksToSSA
import CopyConstantPropagation
import QuadrupleToLLVM
import Gcse

-- COMPILE PROGRAM -------------------------------------------------------------

programToLLVM :: Program -> String
programToLLVM (Program _ topDefs) = programLLVM'
  where
    funResultsMap = genFunResultMap topDefs
    functionsQ = map (funToQuad funResultsMap) topDefs
    processedFunctionsQ = map processFunctionQuadrupleCode functionsQ
    functionsLLVM = map functionToLLVM processedFunctionsQ
    programLLVM = unlines functionsLLVM
    programLLVM' = stdFunctionsDeclarations ++ programLLVM

genFunResultMap :: [TopDef] -> Data.Map.Map String TypeQ
genFunResultMap topDefs = funResultsMapWithStdLib
  where
    nameTypeList = map (\(FnDef _ t (Ident name) _ _) -> (name, declTypeToTypeQ t)) topDefs
    funResultsMap = Data.Map.fromList nameTypeList
    funResultsMapWithStdLib = Data.Map.union funResultsMap stdFunctionsResultTypes

performOptimizations :: [Quadruple] -> [Quadruple]
performOptimizations quadruples = result
  where
    quadruples' = gcse quadruples
    (change, quadruples'') = propagateCopyAndConstant quadruples'
    result = if change
      then
        performOptimizations quadruples''
      else
        quadruples''

-- Perform all quadruple code processing: transform to SSA, optimize (TODO), ... (TODO)
processQuadrupleCode :: [Quadruple] -> [Quadruple]
processQuadrupleCode quadruples = quadruples''
  where
    blocks = divideQuadrupleCodeToBlocks quadruples
    blocks' = blocksToSSA blocks
    quadruples' = blocksToQuadrupleCode blocks'
    quadruples'' = performOptimizations quadruples'

-- Call processQuadrupleCode on function's body
processFunctionQuadrupleCode :: Function -> Function
processFunctionQuadrupleCode (Function t name args quadruples stringLiterals) = Function t name args quadruples' stringLiterals
  where
    quadruples' = processQuadrupleCode quadruples

