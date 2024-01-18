{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

module CopyConstantPropagation (propagateCopyAndConstant) where

import qualified Data.Map

import QuadrupleCode

type PropagationMapping = Data.Map.Map String Value

-- Propagate copy and constant expressions, return True if any changes where made
propagateCopyAndConstant :: [Quadruple] -> (Bool, [Quadruple])
propagateCopyAndConstant code = result
  where
    (code', mapping) = removeAssignments code Data.Map.empty
    code'' = applyPropagationMapping mapping code'
    result = if Data.Map.null mapping
      then
        -- No assignments to propagate where found, return original code
        (False, code)
      else
        -- Some assignments where found, repeat until no more assignments are found
        (True, code''')
          where
            (_, code''') = propagateCopyAndConstant code''

-- Remove unnecessary assignments and return the mapping of values to propagate
removeAssignments :: [Quadruple] -> PropagationMapping -> ([Quadruple], PropagationMapping)
removeAssignments [] mapping = ([], mapping)
removeAssignments (q:tail) mapping = (resQuads, mapping'')
  where
    (isAssignment, updateMapping) = isQuadrupleAssignment q
    mapping' = updateMapping mapping
    (tail', mapping'') = removeAssignments tail mapping'
    resQuads = if isAssignment
      then
        tail'
      else
        q:tail'

isQuadrupleAssignment :: Quadruple -> (Bool, PropagationMapping -> PropagationMapping)
isQuadrupleAssignment (Copy _ (Register dest) src) = (True, Data.Map.insert dest src)
isQuadrupleAssignment (Phi _ destReg@(Register dest) values) = (allEqual, propagation)
  where
    onlyValues = map fst values
    -- First value different then destination register
    firstDifferentValue = head $ filter (/= destReg) onlyValues
    -- True if all values are values different then destination register are equal
    allEqual = all (\val -> val == destReg || val == firstDifferentValue) onlyValues
    propagation = if allEqual
      then
        -- If all values are equal we treat it as a copy with first value different then itself as source (first == all)
        Data.Map.insert dest firstDifferentValue
      else
        id
isQuadrupleAssignment Phi {} = error "isQuadrupleAssignment: Destination of Phi is not a register"
isQuadrupleAssignment _ = (False, id)

-- Apply propagation mapping to code
applyPropagationMapping :: PropagationMapping -> [Quadruple] -> [Quadruple]
applyPropagationMapping mapping = map (applyPropagationMappingToQuadruple mapping)

applyPropagationMappingToQuadruple :: PropagationMapping -> Quadruple -> Quadruple
applyPropagationMappingToQuadruple mapping (Copy t dest src) = Copy t dest (applyMappingToValue mapping src)
applyPropagationMappingToQuadruple mapping (ArithmeticOperation dest t src1 op src2) = ArithmeticOperation dest t src1' op src2'
  where
    src1' = applyMappingToValue mapping src1
    src2' = applyMappingToValue mapping src2
applyPropagationMappingToQuadruple mapping (CompareOperation dest t src1 op src2) = CompareOperation dest t src1' op src2'
  where
    src1' = applyMappingToValue mapping src1
    src2' = applyMappingToValue mapping src2
applyPropagationMappingToQuadruple mapping (ConditionalJump cond trueLabel falseLabel) = ConditionalJump cond' trueLabel falseLabel
  where
    cond' = applyMappingToValue mapping cond
applyPropagationMappingToQuadruple mapping (FunctionCall dest t name args) = FunctionCall dest t name args'
  where
    args' = map (\(FunctionArgument t v) -> FunctionArgument t (applyMappingToValue mapping v)) args
applyPropagationMappingToQuadruple mapping (Return t v) = Return t (applyMappingToValue mapping v)
applyPropagationMappingToQuadruple mapping (Phi t dest values) = Phi t dest values'
  where
    values' = map (\(v, l) -> (applyMappingToValue mapping v, l)) values
applyPropagationMappingToQuadruple _ other = other

applyMappingToValue :: PropagationMapping -> Value -> Value
applyMappingToValue mapping (Register name) = newValue
  where
    maybeNewValue = Data.Map.lookup name mapping
    newValue = case maybeNewValue of
      -- If mapping was successful we have to check if the new value wasn't already mapped to something else
      Just v -> applyMappingToValue mapping v
      Nothing -> Register name
applyMappingToValue _ other = other
