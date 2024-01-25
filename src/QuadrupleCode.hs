module QuadrupleCode where

-- DEFINE QUADRUPLE CODE -------------------------------------------------------

data TypeQ
  = IntQ
  | BoolQ 
  | VoidQ
  | StringQ
  | StructQ String
  deriving (Eq, Show)

data Value 
  = Register String 
  | ConstInt Integer 
  | ConstBool Bool
  | ConstNull String -- Struct type
  deriving (Eq, Show)

type Label = Integer

entryLabel :: Label
entryLabel = 0

firstFreeLabel :: Label
firstFreeLabel = 1

type FunctionName = String

data FunctionArgument = FunctionArgument TypeQ Value
  deriving (Eq, Show)


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
  | ConstString Value String Integer -- destination, name, length
  | GetAttr TypeQ Value String Value Integer -- attr type, destination, object type, object, index
  | SetAttr String Value Integer TypeQ Value Value -- object type, object, index, value type, value, tmp register
  | MethodCall Value TypeQ String Value Integer [FunctionArgument] -- destination, return type, object type, object, index, args
  | NewStruct Value String
  deriving (Eq, Show)

data ArithmeticOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Eq, Show)

data CompareOperator
  = Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq
  deriving (Eq, Show)

-- SIMPLE BLOCK ----------------------------------------------------------------

-- SimpleBlock           block_label  block_code  possible_next_blocks
data SimpleBlock = SimpleBlock Label [Quadruple] [Label] 
  deriving (Show)

-- FUNCTION --------------------------------------------------------------------

--                    ret_type name         args              code        string_constants
data Function = Function TypeQ FunctionName [(TypeQ, String)] [Quadruple] [(String, String)]
  deriving (Show)

-- STRUCT --------------------------------------------------------------------

--                         name   fields  methods
data Structure = Structure String [TypeQ] [String]
