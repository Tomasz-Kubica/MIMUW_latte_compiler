-- module QuadrupleCodeToBlocks (divideQuadrupleCodeToBlocks') where
module QuadrupleCodeToBlocks (divideQuadrupleCodeToBlocks, blocksToQuadrupleCode) where

import QuadrupleCode

-- DIVIDE QUADRUPLE CODE TO BLOCKS ---------------------------------------------

divideQuadrupleCodeToBlocks :: [Quadruple] -> [SimpleBlock]
divideQuadrupleCodeToBlocks quads = blocks'
  where
    blocks = divideQuadrupleCodeToBlocks' quads
    blocks' = removeDeadBlocks blocks

divideQuadrupleCodeToBlocks' :: [Quadruple] -> [SimpleBlock]
-- divideQuadrupleCodeToBlocks' quads  = error ("show quadruples:\n" ++ toShow) -- TODO: remove this line
--   where
--     quadStrings = map show quads
--     toShow = unlines quadStrings
divideQuadrupleCodeToBlocks' [] = []
divideQuadrupleCodeToBlocks' (labelQ@(LabelQ label):tail) = result
  where
    -- SimpleBlock code is saved without label quadruple
    result = case findBlock label tail [] of
      Just (remainingCode, block) -> block:divideQuadrupleCodeToBlocks' remainingCode
      Nothing -> []
-- Dead code after return statement may be generated, it is safe to ignore it (I hope so)
divideQuadrupleCodeToBlocks' (notLabel:tail) = divideQuadrupleCodeToBlocks' tail

--       block_label  code           accumulator    (reaming_code, found_block)
findBlock :: Label -> [Quadruple] -> [Quadruple] -> Maybe ([Quadruple], SimpleBlock)
findBlock label (lastQuad@(Return _ _):tail) acc = Just (tail, SimpleBlock label (accToCode acc lastQuad) [])
findBlock label (lastQuad@ReturnVoid:tail) acc = Just (tail, SimpleBlock label (accToCode acc lastQuad) [])
findBlock label (lastQuad@(Jump jumpLabel):tail) acc = Just (tail, SimpleBlock label (accToCode acc lastQuad) [jumpLabel])
findBlock label (lastQuad@(ConditionalJump _ jumpLabelT jumpLabelF):tail) acc = Just (tail, SimpleBlock label (accToCode acc lastQuad) [jumpLabelT, jumpLabelF])
findBlock label (quad:tail) acc = findBlock label tail (quad:acc)
-- -- This must be an unreachable block at the end of the function, otherwise type check would fail
findBlock _ [] _ = Nothing
-- findBlock _ [] _ = error "findBlock: no return or jump found"
-- -- This must be an unreachable block at the end of the function, otherwise type check would fail
-- findBlock label [] acc = ([], SimpleBlock label acc [])

-- adds last quadruple to akumulator and reverses it
accToCode :: [Quadruple] -> Quadruple -> [Quadruple]
accToCode acc last = reverse (last:acc)

removeDeadBlocks :: [SimpleBlock] -> [SimpleBlock]
removeDeadBlocks blocks = blocks''
  where
    blocksLabels = map (\(SimpleBlock label _ _) -> label) blocks
    (changed, blocks') = filterDead blocks
    blocks'' = if changed then removeDeadBlocks blocks' else blocks'

    -- Block is dead if it only gos to dead (already removed) blocks
    -- This is true because code is correct (type check passed)
    -- If block has no next blocks, it is not dead because it has return statement
    isBlockDead :: SimpleBlock -> Bool
    isBlockDead (SimpleBlock _ _ nextBlocks) = not (any (`elem` blocksLabels) nextBlocks) && nextBlocks /= []

    filterDead :: [SimpleBlock] -> (Bool, [SimpleBlock])
    filterDead [] = (False, [])
    filterDead (h:t) = (changed', h' ++ t')
      where
        hDead = isBlockDead h
        h' = [h | not hDead]
        (changed, t') = filterDead t
        changed' = changed || hDead

-- CONVERT BLOCKS BACK TO QUADRUPLE CODE ---------------------------------------

blocksToQuadrupleCode :: [SimpleBlock] -> [Quadruple]
blocksToQuadrupleCode = concatMap blockToQuadrupleCode

blockToQuadrupleCode :: SimpleBlock -> [Quadruple]
blockToQuadrupleCode (SimpleBlock label code _) = LabelQ label:code
