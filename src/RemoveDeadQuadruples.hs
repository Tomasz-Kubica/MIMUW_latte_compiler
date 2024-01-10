module RemoveDeadQuadruples (removeDeadQuadruples) where

import QuadrupleCode

removeDeadQuadruples :: [Quadruple] -> [Quadruple]
removeDeadQuadruples quads = removeDeadQuadruplesAux quads False

removeDeadQuadruplesAux :: [Quadruple] -> Bool -> [Quadruple]
removeDeadQuadruplesAux [] _ = []
removeDeadQuadruplesAux (quad:tail) remove = quad' ++ tail'
  where
    remove' = not (isLabel quad) && remove
    removeNext = remove' || isReturnOrJump quad
    tail' = removeDeadQuadruplesAux tail removeNext
    quad' = [quad | not remove']

isReturnOrJump :: Quadruple -> Bool
isReturnOrJump (Return _ _) = True
isReturnOrJump (ReturnVoid) = True
isReturnOrJump (Jump _) = True
isReturnOrJump (ConditionalJump _ _ _) = True
isReturnOrJump _ = False

isLabel :: Quadruple -> Bool
isLabel (LabelQ _) = True
isLabel _ = False
