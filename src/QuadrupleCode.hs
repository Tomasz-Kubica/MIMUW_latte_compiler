module QuadrupleCode where

-- DEFINE QUADRUPLE CODE -------------------------------------------------------

data TypeQ
  = IntQ
  | BoolQ 
  deriving (Eq)

data Value = Register String | Constant

type Label = Integer

type FunctionName = String

data FunctionArgument = FunctionArgument TypeQ Value

data Quadruple
  = Copy TypeQ Value Value -- type, destination, source
  | ArithmeticOperation Value TypeQ Value ArithmeticOperator Value
  | CompareOperation Value TypeQ Value CompareOperator Value
  | LabelQ Label
  | Jump Label
  | ConditionalJump Value Label Label -- bool, true_label, false_label
  | FunctionCall Value TypeQ FunctionName [FunctionArgument]
  | Return TypeQ Value
  | ReturnVoid
  | Phi TypeQ [(Value, Label)]

data ArithmeticOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod

data CompareOperator
  = Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq

-- SIMPLE BLOCK ----------------------------------------------------------------

-- SimpleBlock           block_label  block_code  possible_next_blocks
data SimpleBlock = SimpleBlock Label [Quadruple] [Label] 

-- TRANSLATE QUADRUPLE CODE TO LLVM --------------------------------------------
-- TODO: implement
