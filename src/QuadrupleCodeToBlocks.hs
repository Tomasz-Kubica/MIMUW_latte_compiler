-- module QuadrupleCodeToBlocks (divideQuadrupleCodeToBlocks) where
module QuadrupleCodeToBlocks where

import QuadrupleCode

divideQuadrupleCodeToBlocks :: [Quadruple] -> [SimpleBlock]
divideQuadrupleCodeToBlocks [] = []
divideQuadrupleCodeToBlocks (labelQ@(LabelQ label):tail) = block:divideQuadrupleCodeToBlocks remainingCode
  where
    (remainingCode, block) = findBlock label (labelQ:tail) []
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
