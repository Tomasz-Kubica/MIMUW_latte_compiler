module QuadrupleToLLVM (functionToLLVM, structureToLLVM) where

import QuadrupleCode

-- QUADRUPLE CODE TO LLVM ------------------------------------------------------

quadrupleToLLVM :: Quadruple -> String

quadrupleToLLVM (Copy {}) = error "quadrupleToLLVM: Final code should not contain Copy quadruples"

-- Special case for strings, strings addition is concatenation
quadrupleToLLVM (ArithmeticOperation dest StringQ v1 op v2) = quadrupleToLLVM callConcatQuadruple
  where
    -- Call concat function
    callConcatQuadruple = FunctionCall dest StringQ "concat" [FunctionArgument StringQ v1, FunctionArgument StringQ v2]

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
    -- Void function call can't have destination
    llvmDestToUse = if t == VoidQ
      then ""
      else llvmDest ++ " = "
    llvmType = typeToLLVM t
    llvmName = "@" ++ name
    llvmArgs = map (\(FunctionArgument t v) -> typeToLLVM t ++ " " ++ valueToLLVM v) args
    llvmArgsString = joinWithComas llvmArgs
    llvmCode = llvmDestToUse ++ "call " ++ llvmType ++ " " ++ llvmName ++ "(" ++ llvmArgsString ++ ")"

quadrupleToLLVM (Return t value) = addIndent llvmCode
  where
    llvmType = typeToLLVM t
    llvmValue = valueToLLVM value
    llvmCode = "ret " ++ llvmType ++ " " ++ llvmValue

quadrupleToLLVM ReturnVoid = addIndent "ret void"

-- Special case for Void, it is unhallowed and unnecessary
quadrupleToLLVM (Phi VoidQ _ _) = ""

quadrupleToLLVM (Phi t dest values) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmType = typeToLLVM t
    llvmValues = map (\(val, label) -> "[" ++ valueToLLVM val ++ ", %" ++ labelToLLVM label ++ "]") values
    llvmValuesString = joinWithComas llvmValues
    llvmCode = llvmDest ++ " = phi " ++ llvmType ++ " " ++ llvmValuesString

quadrupleToLLVM (ConstString dest name length) = addIndent llvmCode
  where
    llvmDest = valueToLLVM dest
    llvmName = "@." ++ name
    llvmLength = show length
    llvmCode = llvmDest ++ " = getelementptr [" ++ llvmLength ++ " x i8], [" ++ llvmLength ++ " x i8]* " ++ llvmName ++ ", i32 0, i32 0"

-- Structures

quadrupleToLLVM (GetAttr attrT dest structT struct attrIdx) = llvmCode
  where
    llvmStructT = "%" ++ structT
    -- llvmStructPtrT = llvmStructT ++ "*"
    llvmStructPtrT = "ptr"
    llvmStruct = valueToLLVM struct
    llvmGetPtrResult = registerToTmpPtr dest
    llvmGetPtrResultCode = valueToLLVM llvmGetPtrResult
    llvmGetPtr = "getelementptr " ++ llvmStructT ++ ", " ++ llvmStructPtrT ++ " " ++ llvmStruct ++ ", i32 0, i32 " ++ (show attrIdx)
    llvmGetPtrCode = llvmGetPtrResultCode ++ " = " ++ llvmGetPtr
    llvmAtrrT = typeToLLVM attrT
    llvmAtrrTPtr = if llvmAtrrT == "ptr"
      then "ptr"
      else llvmAtrrT ++ "*"
    llvmLoad = "load " ++ llvmAtrrT ++ ", " ++ llvmAtrrTPtr ++ " " ++ llvmGetPtrResultCode
    llvmLoadCode = valueToLLVM dest ++ " = " ++ llvmLoad
    llvmCode = addIndent llvmGetPtrCode ++ "\n" ++ addIndent llvmLoadCode

    registerToTmpPtr :: Value -> Value
    registerToTmpPtr (Register name) = Register (name ++ "_tmp_ptr")

quadrupleToLLVM (SetAttr structT struct attrIdx attrT source tmpReg) = llvmCode
  where
    llvmStructT = "%" ++ structT
    -- llvmStructPtrT = llvmStructT ++ "*"
    llvmStructPtrT = "ptr"
    llvmStruct = valueToLLVM struct
    llvmGetPtrResultCode = valueToLLVM tmpReg
    llvmGetPtr = "getelementptr " ++ llvmStructT ++ ", " ++ llvmStructPtrT ++ " " ++ llvmStruct ++ ", i32 0, i32 " ++ (show attrIdx)
    llvmGetPtrCode = llvmGetPtrResultCode ++ " = " ++ llvmGetPtr
    llvmAtrrT = typeToLLVM attrT
    llvmAtrrTPtr = if llvmAtrrT == "ptr"
      then "ptr"
      else llvmAtrrT ++ "*"
    llvmSource = valueToLLVM source
    llvmStore = "store " ++ llvmAtrrT ++ " " ++ llvmSource ++ ", " ++ llvmAtrrTPtr ++ " " ++ llvmGetPtrResultCode
    llvmCode = addIndent llvmGetPtrCode ++ "\n" ++ addIndent llvmStore

quadrupleToLLVM (NewStruct dest structT) = llvmCode
  where
    -- Allocate memory for struct
    llvmStructT = "%" ++ structT
    llvmStructPtrT = llvmStructT ++ "*"
    llvmDest = valueToLLVM dest
    llvmMalloc = "call ptr @malloc(i64 ptrtoint (ptr getelementptr (" ++ llvmStructT ++ ", ptr null, i32 1) to i64))"
    llvmAlloc = llvmDest ++ " = " ++ llvmMalloc

    -- Set method table pointer
    methodTablePtr = getTmpRegister dest
    llvmMethodTablePtr = valueToLLVM methodTablePtr
    llvmGetTable = llvmMethodTablePtr ++ " = getelementptr " ++ llvmStructT ++ ", ptr " ++ llvmDest ++ ", i32 0, i32 0"
    llvmMethodTable = classNameToMethodTableName structT
    llvmSetTable = "store ptr " ++ llvmMethodTable ++ ", ptr " ++ llvmMethodTablePtr

    -- Join generated code
    llvmCode = unlines (map addIndent [llvmAlloc, llvmGetTable, llvmSetTable])

    getTmpRegister :: Value -> Value
    getTmpRegister (Register name) = Register (name ++ "_tmp_reg_1")
    getTmpRegister _ = error "quadrupleToLLVM for NewStruct: getTmpRegisters: Only registers are allowed"



quadrupleToLLVM (MethodCall dest retT structT struct idx args) = llvmCode
  where
    (methodTablePtr, methodTable, methodPtr, method) = getTmpRegisters dest
    llvmMethodTablePtr = valueToLLVM methodTablePtr
    llvmMethodTable = valueToLLVM methodTable
    llvmMethodPtr = valueToLLVM methodPtr
    llvmMethod = valueToLLVM method

    -- Get method table from object
    llvmStructT = "%" ++ structT
    llvmGetTablePtr = llvmMethodTablePtr ++ " = getelementptr " ++ llvmStructT ++ ", ptr " ++ valueToLLVM struct ++ ", i32 0, i32 0"
    llvmGetTable = llvmMethodTable ++ " = load ptr, ptr " ++ llvmMethodTablePtr

    -- Get method from method table
    llvmGetMethodPtr = llvmMethodPtr ++ " = getelementptr [0 x ptr], ptr " ++ llvmMethodTable ++ ", i32 0, i32 " ++ show idx
    llvmGetMethod = llvmMethod ++ " = load ptr, ptr " ++ llvmMethodPtr

    -- Call method
    llvmDest = valueToLLVM dest
    llvmDestToUse = if retT == VoidQ
      then ""
      else llvmDest ++ " = "
    llvmRetT = typeToLLVM retT
    argsWithSelf = FunctionArgument (StructQ structT) struct : args
    llvmArgs = map (\(FunctionArgument t v) -> typeToLLVM t ++ " " ++ valueToLLVM v) argsWithSelf
    llvmArgsJoined = joinWithComas llvmArgs
    llvmCall = llvmDestToUse ++ "call " ++ llvmRetT ++ " " ++ llvmMethod ++ "(" ++ llvmArgsJoined ++ ")"

    -- Join generated code
    llvmCode = unlines (map addIndent [llvmGetTablePtr, llvmGetTable, llvmGetMethodPtr, llvmGetMethod, llvmCall])

    getTmpRegisters :: Value -> (Value, Value, Value, Value)
    getTmpRegisters (Register name) = (
      Register (name ++ "_tmp_reg_1"),
      Register (name ++ "_tmp_reg_2"),
      Register (name ++ "_tmp_reg_3"),
      Register (name ++ "_tmp_reg_4")
      )

-- Convert type from quadruple code to its LLVM equivalent
typeToLLVM :: TypeQ -> String
typeToLLVM IntQ = "i32"
typeToLLVM BoolQ = "i1"
typeToLLVM VoidQ = "void"
typeToLLVM StringQ = "i8*"
typeToLLVM (StructQ name) = "ptr" --"%" ++ name ++ "*"

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
valueToLLVM (ConstNull struct) = "null" -- TODO: is this correct?
valueToLLVM _ = error "TODO: Constant values not implemented"

labelToLLVM :: Label -> String
labelToLLVM label = "LABEL" ++ show label


-- FUNCTION TO LLVM ------------------------------------------------------------

functionToLLVM :: Function -> String
functionToLLVM (Function t name args body stringLiterals) = llvmCode
  where
    llvmType = typeToLLVM t
    llvmBody = codeToLLVM body
    llvmArgs = map (\(t, name) -> typeToLLVM t ++ " %" ++ name) args
    llvmArgsString = joinWithComas llvmArgs
    llvmHeader = "define " ++ llvmType ++ " @" ++ name ++ "(" ++ llvmArgsString ++ ") {"
    llvmStringLiterals = map stringLiteralToLLVM stringLiterals
    llvmCode = unlines (llvmStringLiterals ++ [llvmHeader, llvmBody, "}"])

codeToLLVM :: [Quadruple] -> String
codeToLLVM code = unlines llvmCode
  where
    llvmCode = map quadrupleToLLVM code

stringLiteralToLLVM :: (String, String) -> String
stringLiteralToLLVM (name, value) = llvmCode
  where
    stringLength = length value + 1 -- +1 for null terminator
    constName = "@." ++ name
    valueWithNull = value ++ "\00"
    llvmCode = constName ++ " = private constant [" ++ show stringLength ++ " x i8] c\"" ++ valueWithNull ++ "\""
  -- @.str = private constant [15 x i8] c"hello, world!\0A\00"


-- STRUCTURE DEFINITION TO LLVM ------------------------------------------------

structureToLLVM :: Structure -> String
structureToLLVM (Structure name attrs methods) = llvmCode
  where
    llvmName = "%" ++ name
    llvmAttrsTypes = map typeToLLVM attrs
    llvmAttrsTypesWithMethodsTablePtr = "ptr" : llvmAttrsTypes
    llvmAttrsTypesString = joinWithComas llvmAttrsTypesWithMethodsTablePtr
    llvmStruct = llvmName ++ " = type { " ++ llvmAttrsTypesString ++ " }"
    llvmTableName = classNameToMethodTableName name
    llvmMethods = map ("ptr @" ++) methods
    llvmMethodTable = llvmTableName ++ " = global [" ++ show (length methods) ++ " x ptr] [" ++ joinWithComas llvmMethods ++ "]"
    llvmCode = unlines [llvmStruct, llvmMethodTable]


-- UTILS -----------------------------------------------------------------------

-- Add appropriate indentation to LLVM code
addIndent :: String -> String
addIndent s = "  " ++ s

classNameToMethodTableName :: String -> String
classNameToMethodTableName name = "@" ++ name ++ ".methods_table"

joinWithComas :: [String] -> String
joinWithComas [] = ""
joinWithComas (h:[]) = h
joinWithComas (h:t) = h ++ ", " ++ joinWithComas t
