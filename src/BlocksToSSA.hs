{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module BlocksToSSA (blocksToSSA) where

import qualified Data.Map
import qualified Data.Set
import Data.List (nub, intersect)
import Control.Monad.State

import QuadrupleCode

-- TRANSFORM TO SSA ------------------------------------------------------------

blocksToSSA :: [SimpleBlock] -> [SimpleBlock]
blocksToSSA = eliminateMultipleAssignments . calculateAndInsertPhiNodes

-- PHI NODES -------------------------------------------------------------------

type VarWithType = (String, TypeQ)

data BlockInfo = BlockInfo {
  block :: SimpleBlock,
  previousBlocks :: [Label],
  phiVariable :: [VarWithType]
}

type InfoMap = Data.Map.Map Label BlockInfo

type VisitedSet = Data.Set.Set Label

-- Calculate and insert phi nodes to blocks
calculateAndInsertPhiNodes :: [SimpleBlock] -> [SimpleBlock]
calculateAndInsertPhiNodes blocks = newBlocks
  where
    -- blocks can't be empty and first block must be an entry point
    firstBlock:_ = blocks
    SimpleBlock firstLabel _ _ = firstBlock
    infoMap = simpleBlocksToInfoMap blocks
    infoMapAfterDFS = calculatePhiNodesDFS Data.Set.empty infoMap firstLabel
    newBlocks = infoMapToSimpleBlocks infoMapAfterDFS

-- Convert list of SimpleBlocks to clean InfoMap
simpleBlocksToInfoMap :: [SimpleBlock] -> InfoMap
simpleBlocksToInfoMap simpleBlocks = infoMap
  where
    infoMap = Data.Map.fromList (map blockToInfo simpleBlocks)
    blockToInfo :: SimpleBlock -> (Label, BlockInfo)
    blockToInfo simpleBlock@(SimpleBlock label _ _) = (label, blockInfo)
      where
        blockInfo = BlockInfo simpleBlock [] []

-- Convert InfoMap to list of SimpleBlocks with phi nodes
infoMapToSimpleBlocks :: InfoMap -> [SimpleBlock]
infoMapToSimpleBlocks infoMap = simpleBlocks
  where
    blockInfos = Data.Map.elems infoMap
    simpleBlocks = map insertPhiNodes blockInfos

-- Add phi nodes to block based on BlockInfo
insertPhiNodes :: BlockInfo -> SimpleBlock
insertPhiNodes blockInfo = newBlock
  where
    SimpleBlock originalBlockLabel originalBlockCode originalBlockNextBlocks = block blockInfo
    previousLabels = previousBlocks blockInfo
    phiVars = phiVariable blockInfo
    phiNodes = map (varToPhi previousLabels) phiVars
    codeWithPhiNodes = phiNodes ++ originalBlockCode
    newBlock = SimpleBlock originalBlockLabel codeWithPhiNodes originalBlockNextBlocks

varToPhi :: [Label] -> (String, TypeQ) -> Quadruple
-- If there are no previous blocks then this shouldn't be called
varToPhi [] _ = error "varToPhi: no previous blocks"
-- In case there is only one previous block copy variable instead of phi node
-- varToPhi [onlyLabel] (varName, varType) = Copy varType (Register varName) (Register varName)
varToPhi prevL (varName, varType) = Phi varType (Register varName) valuesWithLabels
  where
    varAsValue = Register varName
    valuesWithLabels = map (\label -> (varAsValue, label)) prevL

-- Calculate what phi nodes should be added to each block
calculatePhiNodesDFS :: VisitedSet -> InfoMap -> Label -> InfoMap
calculatePhiNodesDFS visited blocksInfo currentBlockLabel = blocksInfoAfterRecursion
  where
    currentBlockInfo = blocksInfo Data.Map.! currentBlockLabel
    currentBlockVariables = getAllVars currentBlockInfo
    -- Update next blocks using variables from current block
    newBlocksInfo = foldl (addVarsToPhiInMap currentBlockLabel currentBlockVariables) blocksInfo nextBlocks
    -- Recursion for next blocks
    SimpleBlock _ _ nextBlocks = block currentBlockInfo
    newVisited = Data.Set.insert currentBlockLabel visited
    unvisitedNextBlocks = filter (\label -> not (Data.Set.member label newVisited)) nextBlocks
    blocksInfoAfterRecursion = foldl (calculatePhiNodesDFS newVisited) newBlocksInfo unvisitedNextBlocks

addVarsToPhiInMap :: Label -> [VarWithType] -> InfoMap -> Label -> InfoMap
addVarsToPhiInMap newLabel newVars blocksInfo keyLabel = newBlocksInfo
  where
    blockInfo = blocksInfo Data.Map.! keyLabel
    newBlockInfo = addVarsToPhi newLabel newVars blockInfo
    newBlocksInfo = Data.Map.insert keyLabel newBlockInfo blocksInfo

-- Update BlockInfo with new variables from one of previous blocks
addVarsToPhi :: Label -> [VarWithType] -> BlockInfo -> BlockInfo
addVarsToPhi label newVars blockInfo = newBlockInfo
  where
    oldPrevBlocks = previousBlocks blockInfo
    newPrevBlocks = label:oldPrevBlocks
    oldPhiVars = phiVariable blockInfo
    newPhiVars = addVars newVars oldPhiVars
    newBlockInfo = blockInfo {
      previousBlocks = newPrevBlocks,
      phiVariable = newPhiVars
    }
    addVars :: [VarWithType] -> [VarWithType] -> [VarWithType]
    addVars new old = if null oldPrevBlocks
      then
        -- Just return new variables 
        new
      else
        -- Return common variables from new and old
        new `intersect` old

-- Get all vars from BlockInfo
getAllVars :: BlockInfo -> [VarWithType]
getAllVars (BlockInfo block _ phiVars) = nub (phiVars ++ extractVariables block)

-- Extract variables from SimpleBlock
extractVariables :: SimpleBlock -> [VarWithType]
extractVariables (SimpleBlock _ code _) = extractVariablesFromCode code

extractVariablesFromCode :: [Quadruple] -> [VarWithType]
extractVariablesFromCode [] = []
extractVariablesFromCode (quad:tail) = quadVars ++ extractVariablesFromCode tail
  where
    quadVars = extractVariablesFromQuad quad

extractVariablesFromQuad :: Quadruple -> [VarWithType]
extractVariablesFromQuad (Copy t (Register name) _) = [(name, t)]
extractVariablesFromQuad (ArithmeticOperation (Register name) t _ _ _) = [(name, t)]
extractVariablesFromQuad (CompareOperation (Register name) _ _ _ _) = [(name, BoolQ)]
extractVariablesFromQuad (FunctionCall (Register name) retType _ _) = [(name, retType)]
extractVariablesFromQuad _ = []

-- ELIMINATION OF MULTIPLE ASSIGNMENTS -----------------------------------------

type BlocksMap = Data.Map.Map Label SimpleBlock

data EliminateState = EliminateState {
  nextID :: Integer,
  blocksMap :: BlocksMap
}

type VarToUniqueVarMap = Data.Map.Map String String

type EliminateMonad a = State EliminateState a

getNextID :: EliminateMonad Integer
getNextID = do
  state <- get
  let id = nextID state
  put (state { nextID = id + 1 })
  return id

eliminateMultipleAssignments :: [SimpleBlock] -> [SimpleBlock]
eliminateMultipleAssignments blocks = blocks'
  where
    blocksM = Data.Map.fromList (map (\(SimpleBlock label code successors) -> (label, SimpleBlock label code successors)) blocks)
    state = EliminateState 0 blocksM
    (_, state') = runState eliminateInAllBlocks state
    blocksM' = blocksMap state'
    blocks' = Data.Map.elems blocksM'

eliminateInAllBlocks :: EliminateMonad ()
eliminateInAllBlocks = do
  state <- get
  let blocksM = blocksMap state
  let labels = Data.Map.keys blocksM
  let eliminateMonads = map eliminateInBlockAndSuccessors labels
  let eliminateMonad = sequence_ eliminateMonads
  eliminateMonad

eliminateInBlockAndSuccessors :: Label -> EliminateMonad ()
eliminateInBlockAndSuccessors label = do
  state <- get
  let blocksM = blocksMap state
  let block = blocksM Data.Map.! label
  mapping <- eliminateInSimpleBlock block
  updateAllPhiNodes label mapping

-- Make all assignments unique in simple block
eliminateInSimpleBlock :: SimpleBlock -> EliminateMonad VarToUniqueVarMap
eliminateInSimpleBlock (SimpleBlock label code nextBlocks) = do
  (code', mapping) <- eliminateInCode code Data.Map.empty
  let block' = SimpleBlock label code' nextBlocks
  state <- get
  let blocksM = blocksMap state
  let blocksM' = Data.Map.insert label block' blocksM
  put (state { blocksMap = blocksM' })
  return mapping

eliminateInCode :: [Quadruple] -> VarToUniqueVarMap -> EliminateMonad ([Quadruple], VarToUniqueVarMap)
eliminateInCode [] mapping = return ([], mapping)
eliminateInCode (quad:tail) mapping = do
  let quad' = applyMappingToQuad quad mapping
  (quad'', changeMapping) <- makeQuadResultUnique quad'
  let mapping' = changeMapping mapping
  (tail', mapping'') <- eliminateInCode tail mapping'
  return (quad'':tail', mapping'')

applyMappingToQuad :: Quadruple -> VarToUniqueVarMap -> Quadruple
applyMappingToQuad (Copy t dest src) mapping = Copy t dest (applyMappingToUsedValue src mapping)
applyMappingToQuad (ArithmeticOperation dest t src1 op src2) mapping = ArithmeticOperation dest t (applyMappingToUsedValue src1 mapping) op (applyMappingToUsedValue src2 mapping)
applyMappingToQuad (CompareOperation dest t src1 op src2) mapping = CompareOperation dest t (applyMappingToUsedValue src1 mapping) op (applyMappingToUsedValue src2 mapping)
applyMappingToQuad (ConditionalJump val l1 l2) mapping = ConditionalJump (applyMappingToUsedValue val mapping) l1 l2
applyMappingToQuad (FunctionCall dest t name args) mapping = FunctionCall dest t name (map (\(FunctionArgument t v) -> FunctionArgument t (applyMappingToUsedValue v mapping)) args)
applyMappingToQuad (Return t val) mapping = Return t (applyMappingToUsedValue val mapping)
applyMappingToQuad quad _ = quad


applyMappingToUsedValue :: Value -> VarToUniqueVarMap -> Value
applyMappingToUsedValue (Register name) mapping = Register (mapping Data.Map.! name)
applyMappingToUsedValue value _ = value

makeQuadResultUnique :: Quadruple -> EliminateMonad (Quadruple, VarToUniqueVarMap -> VarToUniqueVarMap)
makeQuadResultUnique (Copy t dest src) = do
  (dest', mapping) <- makeDestUnique dest
  return (Copy t dest' src, mapping)
makeQuadResultUnique (ArithmeticOperation dest t src1 op src2) = do
  (dest', mapping) <- makeDestUnique dest
  return (ArithmeticOperation dest' t src1 op src2, mapping)
makeQuadResultUnique (CompareOperation dest t src1 op src2) = do
  (dest', mapping) <- makeDestUnique dest
  return (CompareOperation dest' t src1 op src2, mapping)
makeQuadResultUnique (FunctionCall dest t name args) = do
  (dest', mapping) <- makeDestUnique dest
  return (FunctionCall dest' t name args, mapping)
makeQuadResultUnique (Phi t dest values) = do
  (dest', mapping) <- makeDestUnique dest
  return (Phi t dest' values, mapping)
makeQuadResultUnique quad = return (quad, id)

makeDestUnique :: Value -> EliminateMonad (Value, VarToUniqueVarMap -> VarToUniqueVarMap)
makeDestUnique (Register name) = do
  id <- getNextID
  let newName = "u" ++ name ++ "_" ++ show id
  return (Register newName, Data.Map.insert name newName)
makeDestUnique _ = error "makeDestUnique: destination is not register"

-- Update phi nodes in all blocks that are successors of given block
updateAllPhiNodes :: Label -> VarToUniqueVarMap -> EliminateMonad ()
updateAllPhiNodes label mapping = do
  state <- get
  let blocksM = blocksMap state
  let SimpleBlock _ _ nextBlocksLabels = blocksM Data.Map.! label
  let nextBlocks = map (blocksM Data.Map.!) nextBlocksLabels
  let nextBlocks' = map (updateBlockPhiNodes label mapping) nextBlocks
  let blocksM' = foldl insertBlock blocksM nextBlocks'
  let state' = state { blocksMap = blocksM' }
  put state'
    where
      insertBlock :: BlocksMap -> SimpleBlock -> BlocksMap
      insertBlock blocks block@(SimpleBlock l _ _) = Data.Map.insert l block blocks

updateBlockPhiNodes :: Label -> VarToUniqueVarMap -> SimpleBlock -> SimpleBlock
updateBlockPhiNodes label mapping (SimpleBlock l code nextBlocks) = SimpleBlock l code' nextBlocks
  where
    code' = updateCode code
    updateCode :: [Quadruple] -> [Quadruple]
    updateCode [] = []
    updateCode (phi@Phi {}:tail) = phi':tail'
      where
        phi' = updatePhiNode label mapping phi
        tail' = updateCode tail
    -- Update only phi nodes, the rest is left unchanged
    updateCode notPhiCode = notPhiCode

updatePhiNode :: Label -> VarToUniqueVarMap -> Quadruple -> Quadruple
updatePhiNode label mapping (Phi t dest values) = Phi t dest values'
  where
    values' = map mapValue values
    mapValue :: (Value, Label) -> (Value, Label)
    mapValue (phiValue, phiLabel) = if phiLabel == label
      then
        (applyMappingToUsedValue phiValue mapping, phiLabel)
      else
        (phiValue, phiLabel)
updatePhiNode _ _ _ = error "updatePhiNode: not a phi node"
