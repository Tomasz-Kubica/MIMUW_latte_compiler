module QuadrupleToLLVM (functionToLLVM) where

import QuadrupleCode

-- QUADRUPLE CODE TO LLVM ------------------------------------------------------

-- TODO: String !!!
quadrupleToLLVM :: Quadruple -> String

quadrupleToLLVM (Copy {}) = error "quadrupleToLLVM: Final code should not contain Copy quadruples"

quadrupleToLLVM (ArithmeticOperation dest t v1 op v2) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmType = typeToLLVM t
    llvmV1 = valueToLLVM v1
    llvmOp = arithmeticOperatorToLLVM op
    llvmV2 = valueToLLVM v2
    llvmCode = llvmDest ++ " = " ++ llvmOp ++ " " ++ llvmType ++ " " ++ llvmV1 ++ ", " ++ llvmV2

quadrupleToLLVM (CompareOperation dest t v1 op v2) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmType = typeToLLVM t
    llvmV1 = valueToLLVM v1
    llvmOp = compareOperatorToLLVM op
    llvmV2 = valueToLLVM v2
    llvmCode = llvmDest ++ " = icmp " ++ llvmOp ++ " " ++ llvmType ++ " " ++ llvmV1 ++ ", " ++ llvmV2

quadrupleToLLVM (LabelQ label) = labelToLLVM label ++ ":"

quadrupleToLLVM (Jump label) =  addIndent llvmCode
  where
    labelName = labelToLLVM label
    llvmLabel = "label %" ++ labelName
    llvmCode = "br " ++ llvmLabel

quadrupleToLLVM (ConditionalJump condition trueLabel falseLabel) = addIndent llvmCode
  where
    llvmCondition = valueToLLVM condition
    llvmTrueLabel = "label %" ++ labelToLLVM trueLabel
    llvmFalseLabel = "label %" ++ labelToLLVM falseLabel
    llvmCode = "br i1 " ++ llvmCondition ++ ", " ++ llvmTrueLabel ++ ", " ++ llvmFalseLabel

quadrupleToLLVM (FunctionCall dest t name args) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmType = typeToLLVM t
    llvmName = "@" ++ name
    llvmArgs = map (\(FunctionArgument t v) -> typeToLLVM t ++ " " ++ valueToLLVM v) args
    llvmArgsString = joinWithComas llvmArgs
    llvmCode = llvmDest ++ " = call " ++ llvmType ++ " " ++ llvmName ++ "(" ++ llvmArgsString ++ ")"

quadrupleToLLVM (Return t value) = addIndent llvmCode
  where
    llvmType = typeToLLVM t
    llvmValue = valueToLLVM value
    llvmCode = "ret " ++ llvmType ++ " " ++ llvmValue

quadrupleToLLVM ReturnVoid = addIndent "ret void"

quadrupleToLLVM (Phi t dest values) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmType = typeToLLVM t
    llvmValues = map (\(val, label) -> "[" ++ valueToLLVM val ++ ", %" ++ labelToLLVM label ++ "]") values
    llvmValuesString = joinWithComas llvmValues
    llvmCode = llvmDest ++ " = phi " ++ llvmType ++ " " ++ llvmValuesString

-- Convert type from quadruple code to its LLVM equivalent
typeToLLVM :: TypeQ -> String
typeToLLVM IntQ = "i32"
typeToLLVM BoolQ = "i1"

-- Convert arithmetic operator from quadruple code to LLVM operation
arithmeticOperatorToLLVM :: ArithmeticOperator -> String
arithmeticOperatorToLLVM Add = "add"
arithmeticOperatorToLLVM Sub = "sub"
arithmeticOperatorToLLVM Mul = "mul"
arithmeticOperatorToLLVM Div = "sdiv"
arithmeticOperatorToLLVM Mod = "srem"

-- Comparison operator from quadruple code to LLVM operation
compareOperatorToLLVM :: CompareOperator -> String
compareOperatorToLLVM Eq = "eq"
compareOperatorToLLVM Neq = "ne"
compareOperatorToLLVM Lt = "slt"
compareOperatorToLLVM Le = "sle"
compareOperatorToLLVM Gt = "sgt"
compareOperatorToLLVM Ge = "sge"

valueToLLVM :: Value -> String
valueToLLVM (Register name) = "%" ++ name
valueToLLVM (ConstInt x) = show x
valueToLLVM (ConstBool True) = "1"
valueToLLVM (ConstBool False) = "0"
valueToLLVM _ = error "TODO: Constant values not implemented"

labelToLLVM :: Label -> String
labelToLLVM label = "LABEL" ++ show label


-- FUNCTION TO LLVM ------------------------------------------------------------

functionToLLVM :: Function -> String
functionToLLVM (Function t name args body) = llvmCode
  where
    llvmType = typeToLLVM t
    llvmBody = codeToLLVM body
    llvmArgs = map (\(t, name) -> typeToLLVM t ++ " %" ++ name) args
    llvmArgsString = joinWithComas llvmArgs
    llvmHeader = "define " ++ llvmType ++ " @" ++ name ++ "(" ++ llvmArgsString ++ ") {"
    llvmCode = unlines [llvmHeader, llvmBody, "}"]

codeToLLVM :: [Quadruple] -> String
codeToLLVM code = unlines llvmCode
  where
    llvmCode = map quadrupleToLLVM code

-- UTILS -----------------------------------------------------------------------

-- Add appropriate indentation to LLVM code
addIndent :: String -> String
addIndent s = "  " ++ s

joinWithComas :: [String] -> String
joinWithComas [] = ""
joinWithComas (h:[]) = h
joinWithComas (h:t) = h ++ ", " ++ joinWithComas t
