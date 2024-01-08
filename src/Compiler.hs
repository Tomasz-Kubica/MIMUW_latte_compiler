module Compiler where

import qualified Data.Map

import AbsLatte
import QuadrupleCode
import QuadrupleGeneration
import QuadrupleCodeToBlocks
import BlocksToSSA
import QuadrupleToLLVM

-- COMPILE PROGRAM ------------------------------------------------------------_

programToLLVM :: Program -> String
programToLLVM (Program _ topDefs) = programLLVM
  where
    funResultsMap = genFunResultMap topDefs
    functionsQ = map (funToQuad funResultsMap) topDefs
    processedFunctionsQ = map processFunctionQuadrupleCode functionsQ
    functionsLLVM = map functionToLLVM processedFunctionsQ
    programLLVM = unlines functionsLLVM

genFunResultMap :: [TopDef] -> Data.Map.Map String TypeQ
genFunResultMap topDefs = funResultsMap
  where
    nameTypeList = map (\(FnDef _ t (Ident name) _ _) -> (name, declTypeToTypeQ t)) topDefs
    funResultsMap = Data.Map.fromList nameTypeList

-- Perform all quadruple code processing: transform to SSA, optimize (TODO), ... (TODO)
processQuadrupleCode :: [Quadruple] -> [Quadruple]
processQuadrupleCode quadruples = quadruples'
  where
    blocks = divideQuadrupleCodeToBlocks quadruples
    blocks' = blocksToSSA blocks
    quadruples' = blocksToQuadrupleCode blocks'

-- Call processQuadrupleCode on function's body
processFunctionQuadrupleCode :: Function -> Function
processFunctionQuadrupleCode (Function t name args quadruples) = Function t name args quadruples'
  where
    quadruples' = processQuadrupleCode quadruples

