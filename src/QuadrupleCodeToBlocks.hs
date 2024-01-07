-- module QuadrupleCodeToBlocks (divideQuadrupleCodeToBlocks) where
module QuadrupleCodeToBlocks (divideQuadrupleCodeToBlocks, blocksToQuadrupleCode) where

import QuadrupleCode

-- DIVIDE QUADRUPLE CODE TO BLOCKS ---------------------------------------------

divideQuadrupleCodeToBlocks :: [Quadruple] -> [SimpleBlock]
divideQuadrupleCodeToBlocks [] = []
divideQuadrupleCodeToBlocks (labelQ@(LabelQ label):tail) = block:divideQuadrupleCodeToBlocks remainingCode
  where
    -- SimpleBlock code is saved without label quadruple
    (remainingCode, block) = findBlock label tail []
divideQuadrupleCodeToBlocks _ = error "divideQuadrupleCodeToBlocks: first quadruple is not label"

--       block_label  code           accumulator    (reaming_code, found_block)
findBlock :: Label -> [Quadruple] -> [Quadruple] -> ([Quadruple], SimpleBlock)
findBlock label (lastQuad@(Return _ _):tail) acc = (tail, SimpleBlock label (accToCode acc lastQuad) [])
findBlock label (lastQuad@ReturnVoid:tail) acc = (tail, SimpleBlock label (accToCode acc lastQuad) [])
findBlock label (lastQuad@(Jump jumpLabel):tail) acc = (tail, SimpleBlock label (accToCode acc lastQuad) [jumpLabel])
findBlock label (lastQuad@(ConditionalJump _ jumpLabelT jumpLabelF):tail) acc = (tail, SimpleBlock label (accToCode acc lastQuad) [jumpLabelT, jumpLabelF])
findBlock label (quad:tail) acc = findBlock label tail (quad:acc)
findBlock _ [] _ = error "findBlock: no return or jump found"

-- adds last quadruple to akumulator and reverses it
accToCode :: [Quadruple] -> Quadruple -> [Quadruple]
accToCode acc last = reverse (last:acc)

-- CONVERT BLOCKS BACK TO QUADRUPLE CODE ---------------------------------------

blocksToQuadrupleCode :: [SimpleBlock] -> [Quadruple]
blocksToQuadrupleCode = concatMap blockToQuadrupleCode

blockToQuadrupleCode :: SimpleBlock -> [Quadruple]
blockToQuadrupleCode (SimpleBlock label code _) = LabelQ label:code
