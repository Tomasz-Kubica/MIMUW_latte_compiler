module QuadrupleGeneration () where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map

import AbsLatte
import QuadrupleCode

-- Monad -----------------------------------------------------------------------

type TypesMap = Data.Map.Map String TypeQ

data QGMEnv = QGMEnv {
  envVarTypes :: TypesMap,
  envFunResultTypes :: TypesMap
}

data QGMState = QGMState {
  stateIdCounter :: Integer
}

type QGMWrite = [Quadruple]

type QGM a = WriterT QGMWrite (StateT QGMState (ReaderT QGMEnv Identity)) a

runQGM :: QGM a -> QGMEnv -> QGMState -> (a, QGMState, QGMWrite)
runQGM monad env state = (res, state', written)
  where
    ((res, written), state') = runIdentity (runReaderT (runStateT (runWriterT monad) state) env)

-- Monad utility functions -----------------------------------------------------

getNewId :: QGM Integer
getNewId = do
  state <- get
  let id = stateIdCounter state
  put (state { stateIdCounter = id + 1 })
  return id

getTmpRegisterName :: QGM Value
getTmpRegisterName = do
  id <- getNewId
  let registerName = "temp_" ++ show id
  return (Register registerName)

getNewLabel :: QGM Label
getNewLabel = getNewId

addVarType :: String -> TypeQ -> QGMEnv -> QGMEnv
addVarType varName varType env = env {
  envVarTypes = Data.Map.insert varName varType (envVarTypes env)
  }

-- Quadruple generation --------------------------------------------------------

generateQuadrupleCodeExp :: Expr -> QGM (Value, TypeQ)

-- Nothing to do just return variable as value
generateQuadrupleCodeExp (EVar _ varName) = do
  let expT = IntQ -- TODO: get var type from env
  return (Register (show varName), expT)

-- Arithmetic expressions

generateQuadrupleCodeExp (EAdd _ e1 op e2) = do
  (v1, t1) <- generateQuadrupleCodeExp e1
  (v2, _t2) <- generateQuadrupleCodeExp e2
  tmpRegisterName <- getTmpRegisterName
  tell [ArithmeticOperation tmpRegisterName t1 v1 Add v2]
  return (tmpRegisterName, t1)

-- Boolean expressions

generateQuadrupleCodeExp (ELitTrue _) = do
  tmpRegisterName <- getTmpRegisterName
  tell [Copy BoolQ tmpRegisterName Constant] -- TODO: set constant to true
  return (tmpRegisterName, BoolQ)

generateQuadrupleCodeExp (ELitFalse _) = do
  tmpRegisterName <- getTmpRegisterName
  tell [Copy BoolQ tmpRegisterName Constant] -- TODO: set constant to false
  return (tmpRegisterName, BoolQ)

generateQuadrupleCodeExp e@EAnd {} = do
  v <- generateBoolExpr e
  return (v, BoolQ)

generateQuadrupleCodeExp e@EOr {} = do
  v <- generateBoolExpr e
  return (v, BoolQ)

generateQuadrupleCodeExp e@ERel {} = do
  v <- generateBoolExpr e
  return (v, BoolQ)

generateQuadrupleCodeExp e@Not {} = do
  v <- generateBoolExpr e
  return (v, BoolQ)

-- Boolean expressions lazy evaluation

-- :: exp -> trueLabel -> falseLabel
generateQuadrupleCodeCond :: Expr -> Label -> Label -> QGM ()

generateQuadrupleCodeCond (EAnd _ e1 e2) trueL falseL = do
  newLabel <- getNewLabel
  let newLabelQuadruple = LabelQ newLabel
  generateQuadrupleCodeCond e1 newLabel falseL
  tell [newLabelQuadruple]
  generateQuadrupleCodeCond e2 trueL falseL

generateQuadrupleCodeCond (EOr _ e1 e2) trueL falseL = do
  newLabel <- getNewLabel
  let newLabelQuadruple = LabelQ newLabel
  generateQuadrupleCodeCond e1 trueL newLabel
  tell [newLabelQuadruple]
  generateQuadrupleCodeCond e2 trueL falseL

generateQuadrupleCodeCond (Not _ e) trueL falseL = generateQuadrupleCodeCond e falseL trueL

generateQuadrupleCodeCond (ERel _ e1 rOp e2) trueL falseL = do
  let quadOp = absOpToQuadOp rOp
  (v1, t1) <- generateQuadrupleCodeExp e1
  (v2, _t2) <- generateQuadrupleCodeExp e2
  tmpRegisterName <- getTmpRegisterName
  tell [
    CompareOperation tmpRegisterName t1 v1 quadOp v2,
    ConditionalJump tmpRegisterName trueL falseL
    ]

generateQuadrupleCodeCond (ELitTrue _) trueL falseL = do
  tell [Jump trueL]

generateQuadrupleCodeCond (ELitFalse _) trueL falseL = do
  tell [Jump falseL]

generateBoolExpr :: Expr -> QGM Value
generateBoolExpr e = do
  trueL <- getNewLabel
  falseL <- getNewLabel
  endL <- getNewLabel
  resultRegister <- getTmpRegisterName
  generateQuadrupleCodeCond e trueL falseL
  tell [
    LabelQ trueL,
    Copy BoolQ resultRegister Constant, -- TODO: set constant to true
    Jump endL,
    LabelQ falseL,
    Copy BoolQ resultRegister Constant, -- TODO: set constant to false
    Jump endL,
    LabelQ endL
    ]
  return resultRegister

absOpToQuadOp :: RelOp -> CompareOperator
absOpToQuadOp (LTH _) = Lt
absOpToQuadOp (LE _) = Le
absOpToQuadOp (GTH _) = Gt
absOpToQuadOp (GE _) = Ge
absOpToQuadOp (EQU _) = Eq
absOpToQuadOp (NE _) = Neq

generateQuadrupleCodeStmt :: Stmt -> QGM (QGMEnv -> QGMEnv)

generateQuadrupleCodeStmt (Empty _) = return id

generateQuadrupleCodeStmt (BStmt _ (Block _ block)) = do
  generateQuadrupleCodeStmtsList block
  return id

generateQuadrupleCodeStmt (Decl _ declType items) = do
  let typeQ = declTypeToTypeQ declType
  generateQuadrupleCodeVarDecls typeQ items
  where
    declTypeToTypeQ (Int _) = IntQ
    declTypeToTypeQ (Bool _) = BoolQ

generateQuadrupleCodeStmt (Ass _ (EVar _ varName) source) = do
  (v, t) <- generateQuadrupleCodeExp source
  tell [Copy t (Register (show varName)) v]
  return id


generateQuadrupleCodeStmt (Incr _ varName) = do
  let varType = IntQ -- TODO: get type from env
  let varRegister = Register (show varName)
  tell [
    ArithmeticOperation varRegister varType varRegister Add Constant -- TODO: set constant to 1
    ]
  return id

generateQuadrupleCodeStmt (Decr _ varName) = do
  let varType = IntQ -- TODO: get type from env
  let varRegister = Register (show varName)
  tell [
    ArithmeticOperation varRegister varType varRegister Sub Constant -- TODO: set constant to 1
    ]
  return id

generateQuadrupleCodeStmt (Ret _ e) = do
  (v, t) <- generateQuadrupleCodeExp e
  tell [Return t v]
  return id

generateQuadrupleCodeStmt (VRet _) = do
  tell [ReturnVoid]
  return id

generateQuadrupleCodeStmt (Cond _ cond body) = do
  bodyL <- getNewLabel
  endL <- getNewLabel
  generateQuadrupleCodeCond cond bodyL endL
  tell [LabelQ bodyL]
  generateQuadrupleCodeStmt body
  tell [
    Jump endL,
    LabelQ endL
    ]
  return id

generateQuadrupleCodeStmt (CondElse _ cond bodyT bodyF) = do
  trueL <- getNewLabel
  falseL <- getNewLabel
  endL <- getNewLabel
  generateQuadrupleCodeCond cond trueL falseL
  -- generate code for true branch
  tell [LabelQ trueL]
  generateQuadrupleCodeStmt bodyT
  tell [Jump endL]
  -- generate code for false branch
  tell [LabelQ falseL]
  generateQuadrupleCodeStmt bodyF
  tell [Jump endL]
  -- generate end label
  tell [LabelQ endL]
  return id

generateQuadrupleCodeStmt (While _ cond body) = do
  condL <- getNewLabel
  bodyL <- getNewLabel
  endL <- getNewLabel
  -- generate code for condition
  tell [
    Jump condL,
    LabelQ condL
    ]
  generateQuadrupleCodeCond cond bodyL endL
  -- generate code for body
  tell [LabelQ bodyL]
  generateQuadrupleCodeStmt body
  -- generate code for returning to condition and end label
  tell [
    Jump condL,
    LabelQ endL
    ]
  return id

generateQuadrupleCodeStmt (SExp _ e) = do
  -- generate code for calculating expression and ignoring result
  generateQuadrupleCodeExp e
  return id


-- Function for generating code for statements lists
generateQuadrupleCodeStmtsList :: [Stmt] -> QGM ()
generateQuadrupleCodeStmtsList [] = return ()
generateQuadrupleCodeStmtsList (stmt:tail) = do
  changeEnv <- generateQuadrupleCodeStmt stmt
  local changeEnv (generateQuadrupleCodeStmtsList tail)

-- Function for generating code for variable declarations
generateQuadrupleCodeVarDecl :: TypeQ -> Item -> QGM (QGMEnv -> QGMEnv)
generateQuadrupleCodeVarDecl varType (NoInit _ (Ident varName)) = do
  let varRegister = Register (show varName)
  let value = typeToInitVal varType
  tell [Copy varType varRegister value]
  return (addVarType varName varType)
    where
      typeToInitVal IntQ = Constant -- TODO: set constant to 0
      typeToInitVal _ = Constant -- TODO: add default values for other types

generateQuadrupleCodeVarDecl varType (Init _ (Ident varName) e) = do
  (v, _) <- generateQuadrupleCodeExp e
  tell [Copy t (Register (show varName)) v]
  return (addVarType varName varType)

generateQuadrupleCodeVarDecls :: TypeQ -> [Item] -> QGM (QGMEnv -> QGMEnv)
generateQuadrupleCodeVarDecls varType [] = return id
generateQuadrupleCodeVarDecls varType (item:tail) = do
  changeEnv <- generateQuadrupleCodeVarDecl varType item
  changeEnvTail <- local changeEnv (generateQuadrupleCodeVarDecls varType tail)
  return (changeEnvTail . changeEnv) -- check order of composition
