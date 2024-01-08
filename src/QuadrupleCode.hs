module QuadrupleCode where

-- DEFINE QUADRUPLE CODE -------------------------------------------------------

data TypeQ
  = IntQ
  | BoolQ 
  | VoidQ
  deriving (Eq, Show)

data Value 
  = Register String 
  | ConstInt Integer 
  | ConstBool Bool
  deriving (Eq, Show)

type Label = Integer

entryLabel :: Label
entryLabel = 0

firstFreeLabel :: Label
firstFreeLabel = 1

type FunctionName = String

data FunctionArgument = FunctionArgument TypeQ Value
  deriving (Show)


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
  | Phi TypeQ Value [(Value, Label)]
  deriving (Show)

data ArithmeticOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show)

data CompareOperator
  = Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq
  deriving (Show)

-- SIMPLE BLOCK ----------------------------------------------------------------

-- SimpleBlock           block_label  block_code  possible_next_blocks
data SimpleBlock = SimpleBlock Label [Quadruple] [Label] 

-- FUNCTION --------------------------------------------------------------------

data Function = Function TypeQ FunctionName [(TypeQ, String)] [Quadruple]

-- TRANSLATE QUADRUPLE CODE TO LLVM --------------------------------------------
-- TODO: implement
