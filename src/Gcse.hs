module Gcse (gcse) where

import qualified Data.Map
import qualified Data.IntMap
import Data.Tree
import Control.Monad.State

import QuadrupleCode
import QuadrupleCodeToBlocks
import DominatorTree

type ExprMapping = [Quadruple]

data GCSEState = GCSEState {
  stateBlocks :: Data.Map.Map Label SimpleBlock
}

type GCSEMonad a = State GCSEState a

runGCSEMonad :: GCSEMonad a -> GCSEState -> (a, GCSEState)
runGCSEMonad = runState

-- GCSE ------------------------------------------------------------------------

gcse :: [Quadruple] -> [Quadruple]
gcse quadruples = quadruples'
  where
    blocks = divideQuadrupleCodeToBlocks quadruples
    domTree = generateDominatorTree blocks getLabel getNextBlocks
    blocksMap = blocksToMap blocks
    state = GCSEState {stateBlocks = blocksMap}
    (_, state') = runGCSEMonad (gcseMonad [] domTree) state
    blocksMap' = stateBlocks state'
    blocks' = mapToBlocks blocksMap'
    quadruples' = blocksToQuadrupleCode blocks'

    blocksToMap :: [SimpleBlock] -> Data.Map.Map Label SimpleBlock
    blocksToMap blocks = Data.Map.fromList $ map (\block -> (getLabel block, block)) blocks

    mapToBlocks :: Data.Map.Map Label SimpleBlock -> [SimpleBlock]
    mapToBlocks map = Data.Map.elems map

    getLabel :: SimpleBlock -> Label
    getLabel (SimpleBlock label _ _) = label

    getNextBlocks :: SimpleBlock -> [Label]
    getNextBlocks (SimpleBlock _ _ nextBlocks) = nextBlocks

gcseMonad :: ExprMapping -> DomTree Label -> GCSEMonad ()
gcseMonad mapping domTree = do
  let tree = domTreeTree domTree
  let labelMapping = domTreeMapping domTree
  let rLabel = rootLabel tree
  let rootOriginalLabel = labelMapping Data.IntMap.! rLabel
  mapping' <- lcseMonad mapping rootOriginalLabel
  let childrenTrees = subForest tree
  let childrenDomTrees = map (\tree -> domTree {domTreeTree = tree}) childrenTrees
  mapM_ (gcseMonad mapping') childrenDomTrees
  

-- LCSE ------------------------------------------------------------------------

lcseMonad :: ExprMapping -> Label -> GCSEMonad ExprMapping
lcseMonad mapping label = do
  blocks <- gets stateBlocks
  let block = blocks Data.Map.! label
  let (block', mapping') = lcseBlock mapping block
  let blocks' = Data.Map.insert label block' blocks
  modify (\state -> state {stateBlocks = blocks'})
  return mapping'

lcseBlock :: ExprMapping -> SimpleBlock -> (SimpleBlock, ExprMapping)
lcseBlock mapping (SimpleBlock label code nextBlocks) = (SimpleBlock label code' nextBlocks, mapping')
  where
    (code', mapping') = lcse mapping code

lcse :: ExprMapping -> [Quadruple] -> ([Quadruple], ExprMapping)

lcse mapping [] = ([], mapping)

lcse mapping (quad:tail) = (quad':tail', mapping'')
  where
    quad' = applyMapping mapping quad
    mapping' = addToMapping mapping quad'
    (tail', mapping'') = lcse mapping' tail


-- UTILITY FOR QUADRUPLE CODE FOR LCSE and GCSE --------------------------------

-- Only quadruples that GCSE can be applied to are compared
compareQuadrupleExpressions :: Quadruple -> Quadruple -> Bool

compareQuadrupleExpressions (ArithmeticOperation _dest1 t1 src11 op1 src12) (ArithmeticOperation _dest2 t2 src21 op2 src22) = result
  where
    q1NoDest = ArithmeticOperation (Register "dummy") t1 src11 op1 src12
    q2NoDest = ArithmeticOperation (Register "dummy") t2 src21 op2 src22
    result = q1NoDest == q2NoDest

compareQuadrupleExpressions (CompareOperation _dest1 t1 src11 op1 src12) (CompareOperation _dest2 t2 src21 op2 src22) = result
  where
    q1NoDest = CompareOperation (Register "dummy") t1 src11 op1 src12
    q2NoDest = CompareOperation (Register "dummy") t2 src21 op2 src22
    result = q1NoDest == q2NoDest

-- GCSE cant't be applied to other quadruples, so no need to compare them
compareQuadrupleExpressions _ _ = False

-- Only quadruples that GCSE can be applied to
getQuadrupleDest :: Quadruple -> (Value, TypeQ)
getQuadrupleDest (ArithmeticOperation dest t _ _ _) = (dest, t)
getQuadrupleDest (CompareOperation dest t _ _ _) = (dest, t)

applyMapping :: ExprMapping -> Quadruple -> Quadruple
applyMapping [] quad = quad
applyMapping (savedQuad:tail) quad = if compareQuadrupleExpressions savedQuad quad
  then
    Copy destType quadDest savedQuadDest
  else
    applyMapping tail quad
    where
      (quadDest, destType) = getQuadrupleDest quad
      (savedQuadDest, _) = getQuadrupleDest savedQuad

addToMapping :: ExprMapping -> Quadruple -> ExprMapping
addToMapping mapping quad@ArithmeticOperation {} = quad:mapping
addToMapping mapping quad@CompareOperation {} = quad:mapping
addToMapping mapping _ = mapping
