module Gcse (gcse) where

import qualified Data.Map
import qualified Data.IntMap
import Data.Tree
import Control.Monad.State

import QuadrupleCode
import QuadrupleCodeToBlocks
import DominatorTree

data MappedExpr = MappedExpr {
  mappedExprQuad :: Quadruple,
  mappedExprPath :: [Int],
  mappedExprNoInBlock :: Int
}

type ExprMapping = [MappedExpr]

data GCSEState = GCSEState {
  stateBlocks :: Data.Map.Map Label SimpleBlock,
  stateExprMapping :: ExprMapping
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
    state = GCSEState {stateBlocks = blocksMap, stateExprMapping = []}
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

gcseMonad :: [Int] -> DomTree Label -> GCSEMonad ()
gcseMonad path domTree = do
  let tree = domTreeTree domTree
  let labelMapping = domTreeMapping domTree
  let rLabel = rootLabel tree
  let path' = rLabel : path
  
  gcseBlock labelMapping path' rLabel
  
  let childrenTrees = subForest tree
  let childrenDomTrees = map (\tree -> domTree {domTreeTree = tree}) childrenTrees
  mapM_ (gcseMonad path') childrenDomTrees

gcseBlock :: Data.IntMap.IntMap Label -> [Int] -> Int -> GCSEMonad ()
gcseBlock labelMapping path treeId = do
  let label = labelMapping Data.IntMap.! treeId
  blocks <- gets stateBlocks
  let SimpleBlock _ bCode _ = blocks Data.Map.! label
  gcseBlockCode labelMapping 0 path bCode

gcseBlockCode :: Data.IntMap.IntMap Label -> Int -> [Int] -> [Quadruple] -> GCSEMonad ()
gcseBlockCode _ _ _ [] = return ()
gcseBlockCode labelMapping quadNo path (quad:tail) = do
  let quadMapped = MappedExpr {mappedExprQuad = quad, mappedExprPath = path, mappedExprNoInBlock = quadNo}
  savedExprs <- gets stateExprMapping
  let foundMapping = findMapping savedExprs quad

  case foundMapping of
    Just foundMapped -> do
      -- matching expression found apply mapping
      applyMapping labelMapping foundMapped quadMapped
    Nothing -> do
      -- New expr save it in mapping
      let savedExprs' = addToMapping savedExprs quadMapped
      modify (\state -> state {stateExprMapping = savedExprs'})
    
  gcseBlockCode labelMapping (quadNo + 1) path tail

applyMapping :: Data.IntMap.IntMap Label -> MappedExpr -> MappedExpr -> GCSEMonad ()
applyMapping labelMapping mapped1 mapped2 = do
  let path1 = mappedExprPath mapped1
  let path2 = mappedExprPath mapped2
  let id1 = head path1
  let id2 = head path2
  let inBlockNo1 = mappedExprNoInBlock mapped1
  let inBlockNo2 = mappedExprNoInBlock mapped2
  let quad1 = mappedExprQuad mapped1
  let quad2 = mappedExprQuad mapped2
  let (quad1Dest, destType) = getQuadrupleDest quad1
  let (quad2Dest, _) = getQuadrupleDest quad2

  let commonAncestors = lca path1 path2
  let lca = head commonAncestors
  let lcaPath = reverse commonAncestors

  let label1 = labelMapping Data.IntMap.! id1
  let label2 = labelMapping Data.IntMap.! id2
  let labelLca = labelMapping Data.IntMap.! lca

  if lca == id1 && (lca /= id2 || inBlockNo1 < inBlockNo2)
    then do
    -- use value from mapped1 in mapped2
      let newQuad2 = Copy destType quad2Dest quad1Dest
      replaceQuadruple quad2 newQuad2 label2
    else if lca == id2 && (lca /= id1 || inBlockNo2 < inBlockNo1) then do
    -- use value from mapped2 in mapped1
      let newQuad1 = Copy destType quad1Dest quad2Dest
      replaceQuadruple quad1 newQuad1 label1
    else do
    -- extract operation to common ancestor and use it in both mapped1 and mapped2
      let newRegister = mergeRegisterNames quad1Dest quad2Dest
      let newCommonQuad = changeQuadrupleDest quad1 newRegister
      let newQuad1 = Copy destType quad1Dest newRegister
      let newQuad2 = Copy destType quad2Dest newRegister
      replaceQuadruple quad1 newQuad1 label1
      replaceQuadruple quad2 newQuad2 label2
      newQuadInBlockNo <- addQuadruple newCommonQuad labelLca
      -- remove old quadruples from mapping and add new one
      savedExprs <- gets stateExprMapping
      let savedExprs' = removeFromMapping savedExprs mapped1
      let savedExprs'' = removeFromMapping savedExprs' mapped2
      let newMapped = MappedExpr {mappedExprQuad = newCommonQuad, mappedExprPath = lcaPath, mappedExprNoInBlock = newQuadInBlockNo}
      let savedExprs''' = addToMapping savedExprs'' newMapped
      modify (\state -> state {stateExprMapping = savedExprs'''})


replaceQuadruple :: Quadruple -> Quadruple -> Label -> GCSEMonad ()
replaceQuadruple oldQuad newQuad label = do
  blocks <- gets stateBlocks
  let SimpleBlock bLabel bCode bNextBlocks = blocks Data.Map.! label
  let bCode' = replaceQuadrupleAux oldQuad newQuad bCode
  let newBlock = SimpleBlock bLabel bCode' bNextBlocks
  let blocks' = Data.Map.insert label newBlock blocks
  modify (\state -> state {stateBlocks = blocks'})

replaceQuadrupleAux :: Quadruple -> Quadruple -> [Quadruple] -> [Quadruple]
replaceQuadrupleAux _ _ [] = []
replaceQuadrupleAux oldQuad newQuad (quad:tail) = if oldQuad == quad
  then
    newQuad : tail
  else
    quad : replaceQuadrupleAux oldQuad newQuad tail

addQuadruple :: Quadruple -> Label -> GCSEMonad Int
addQuadruple quad label = do
  blocks <- gets stateBlocks
  let SimpleBlock bLabel bCode bNextBlocks = blocks Data.Map.! label
  let bCode' = addAsPenultimate bCode quad
  let newBlock = SimpleBlock bLabel bCode' bNextBlocks
  let blocks' = Data.Map.insert label newBlock blocks
  modify (\state -> state {stateBlocks = blocks'})
  return (length bCode)
    where
      addAsPenultimate :: [a] -> a -> [a]
      addAsPenultimate [] _ = error "Can't add as penultimate to empty list"
      addAsPenultimate [x] y = [y, x]
      addAsPenultimate (x:tail) y = x : addAsPenultimate tail y
  

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

-- Only quadruples that GCSE can be used here
getQuadrupleDest :: Quadruple -> (Value, TypeQ)
getQuadrupleDest (ArithmeticOperation dest t _ _ _) = (dest, t)
getQuadrupleDest (CompareOperation dest t _ _ _) = (dest, t)

-- Only quadruples that GCSE can be used here
changeQuadrupleDest :: Quadruple -> Value -> Quadruple
changeQuadrupleDest (ArithmeticOperation _ t src1 op src2) dest = ArithmeticOperation dest t src1 op src2
changeQuadrupleDest (CompareOperation _ t src1 op src2) dest = CompareOperation dest t src1 op src2

findMapping :: ExprMapping -> Quadruple -> Maybe MappedExpr
findMapping [] _ = Nothing
findMapping (savedMapped:tail) quad = if compareQuadrupleExpressions savedQuad quad
  then
    Just savedMapped
  else
    findMapping tail quad
  where
    savedQuad = mappedExprQuad savedMapped
    (quadDest, destType) = getQuadrupleDest quad
    (savedQuadDest, _) = getQuadrupleDest savedQuad

addToMapping :: ExprMapping -> MappedExpr -> ExprMapping
addToMapping mapping mapped = if isToSave quad
  then
    mapped : mapping
  else
    mapping
  where
    quad = mappedExprQuad mapped
    isToSave :: Quadruple -> Bool
    isToSave ArithmeticOperation {} = True
    isToSave CompareOperation {} = True
    isToSave _ = False

removeFromMapping :: ExprMapping -> MappedExpr -> ExprMapping
removeFromMapping [] _ = []
removeFromMapping (savedMapped:tail) mapped = if compareQuadrupleExpressions savedQuad quad
  then
    tail
  else
    savedMapped : removeFromMapping tail mapped
  where
    savedQuad = mappedExprQuad savedMapped
    quad = mappedExprQuad mapped


mergeRegisterNames :: Value -> Value -> Value
mergeRegisterNames (Register name1) (Register name2) = Register ("merged_" ++ name1 ++ "_" ++ name2)
mergeRegisterNames _ _ = error "Can't merge values different then registers"

-- LCA -------------------------------------------------------------------------

-- returns list of common ancestors LCA is first on the list
lca :: [Int] -> [Int] -> [Int]
lca a b = lcaRev [] (reverse a) (reverse b)

-- lca on reverse path (root first)
lcaRev :: [Int] -> [Int] -> [Int] -> [Int]
lcaRev lastKnownCA [] _ = lastKnownCA
lcaRev lastKnownCA _ [] = lastKnownCA
lcaRev lastKnownCA (x:xs) (y:ys) = if x == y
  then
    lcaRev (x:lastKnownCA) xs ys
  else
    lastKnownCA
