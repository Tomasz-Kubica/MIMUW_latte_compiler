{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module QuadrupleGeneration (funToQuad, declTypeToTypeQ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map

import AbsLatte
import QuadrupleCode

-- Monad -----------------------------------------------------------------------

-- Maps variables to their names in quadruple code and their types
type TypesMap = Data.Map.Map String (String, TypeQ)

type ResultTypesMap = Data.Map.Map String TypeQ

data QGMEnv = QGMEnv {
  envVarTypes :: TypesMap,
  envFunResultTypes :: ResultTypesMap
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

-- Monad constants -------------------------------------------------------------

startQGMState :: QGMState
startQGMState = QGMState {
  stateIdCounter = firstFreeLabel
}

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

getNewVariableQuadName :: QGM String
getNewVariableQuadName = do
  id <- getNewId
  let varName = "var_" ++ show id
  return varName

getNewLabel :: QGM Label
getNewLabel = getNewId

addVarType :: String -> String ->TypeQ -> QGMEnv -> QGMEnv
addVarType varName varQuadName varType env = env {
  envVarTypes = Data.Map.insert varName (varQuadName, varType) (envVarTypes env)
  }

getVarQuadNameAndType :: String -> QGM (String, TypeQ)
getVarQuadNameAndType varName = do
  env <- ask
  let maybeResult = Data.Map.lookup varName (envVarTypes env)
  let result = unwrapMaybe maybeResult
  return result
    where
      unwrapMaybe (Just x) = x
      unwrapMaybe Nothing = error "getVarQuadNameAndType: variable not found"

-- Quadruple generation --------------------------------------------------------

generateQuadrupleCodeExp :: Expr -> QGM (Value, TypeQ)

-- Nothing to do just return variable as value
generateQuadrupleCodeExp (EVar _ (Ident varName)) = do
  (varQuadName, varType) <- getVarQuadNameAndType varName
  return (Register varQuadName, varType)

-- Arithmetic expressions

generateQuadrupleCodeExp (EAdd _ e1 op e2) = do
  (v1, t1) <- generateQuadrupleCodeExp e1
  (v2, _t2) <- generateQuadrupleCodeExp e2
  tmpRegisterName <- getTmpRegisterName
  tell [ArithmeticOperation tmpRegisterName t1 v1 opQ v2]
  return (tmpRegisterName, t1)
    where
      opQ = opToQuadOp op
      opToQuadOp :: AddOp -> ArithmeticOperator
      opToQuadOp (AbsLatte.Plus _) = QuadrupleCode.Add
      opToQuadOp (AbsLatte.Minus _) = QuadrupleCode.Sub

generateQuadrupleCodeExp (EMul _ e1 op e2) = do
  (v1, t1) <- generateQuadrupleCodeExp e1
  (v2, _t2) <- generateQuadrupleCodeExp e2
  tmpRegisterName <- getTmpRegisterName
  tell [ArithmeticOperation tmpRegisterName t1 v1 opQ v2]
  return (tmpRegisterName, t1)
    where
      opQ = opToQuadOp op
      opToQuadOp :: MulOp -> ArithmeticOperator
      opToQuadOp (AbsLatte.Times _) = QuadrupleCode.Mul
      opToQuadOp (AbsLatte.Div _) = QuadrupleCode.Div
      opToQuadOp (AbsLatte.Mod _) = QuadrupleCode.Mod


generateQuadrupleCodeExp (ELitInt _ x) = do
  return (ConstInt x, IntQ)

-- Function call

generateQuadrupleCodeExp (EApp _ (Ident funName) args) = do
  env <- ask
  let resTypeMap = envFunResultTypes env
  let retType = resTypeMap Data.Map.! funName
  calculatedArgs <- mapM generateQuadrupleCodeExp args
  let argsQ = map (\(v, t) -> FunctionArgument t v) calculatedArgs
  tmpRegisterName <- getTmpRegisterName
  tell [FunctionCall tmpRegisterName retType funName argsQ]
  return (tmpRegisterName, retType)


-- Boolean expressions

generateQuadrupleCodeExp (ELitTrue _) = do
  tmpRegisterName <- getTmpRegisterName
  tell [Copy BoolQ tmpRegisterName (ConstBool True)]
  return (tmpRegisterName, BoolQ)

generateQuadrupleCodeExp (ELitFalse _) = do
  tmpRegisterName <- getTmpRegisterName
  tell [Copy BoolQ tmpRegisterName (ConstBool False)]
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
    Copy BoolQ resultRegister (ConstBool True),
    Jump endL,
    LabelQ falseL,
    Copy BoolQ resultRegister (ConstBool False),
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

declTypeToTypeQ :: Type -> TypeQ
declTypeToTypeQ (Int _) = IntQ
declTypeToTypeQ (Bool _) = BoolQ
declTypeToTypeQ (Void _) = VoidQ

generateQuadrupleCodeStmt :: Stmt -> QGM (QGMEnv -> QGMEnv)

generateQuadrupleCodeStmt (Empty _) = return id

generateQuadrupleCodeStmt (BStmt _ (Block _ block)) = do
  generateQuadrupleCodeStmtsList block
  return id

generateQuadrupleCodeStmt (Decl _ declType items) = do
  let typeQ = declTypeToTypeQ declType
  generateQuadrupleCodeVarDecls typeQ items

generateQuadrupleCodeStmt (Ass _ (EVar _ (Ident varName)) source) = do
  (v, t) <- generateQuadrupleCodeExp source
  (quadName, _) <- getVarQuadNameAndType varName
  tell [Copy t (Register quadName) v]
  return id

generateQuadrupleCodeStmt (Incr _ (Ident varName)) = do
  (quadName, varType) <- getVarQuadNameAndType varName
  let varRegister = Register quadName
  tell [
    ArithmeticOperation varRegister varType varRegister Add (ConstInt 1)
    ]
  return id

generateQuadrupleCodeStmt (Decr _ (Ident varName)) = do
  (quadName, varType) <- getVarQuadNameAndType varName
  let varRegister = Register quadName
  tell [
    ArithmeticOperation varRegister varType varRegister Sub (ConstInt 1)
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
  varQuadName <- getNewVariableQuadName
  let varRegister = Register varQuadName
  let value = typeToInitVal varType
  tell [Copy varType varRegister value]
  return (addVarType varName varQuadName varType)
    where
      typeToInitVal IntQ = ConstInt 0
      typeToInitVal BoolQ = ConstBool False
      typeToInitVal _ = error "TODO: add default values for other types"

generateQuadrupleCodeVarDecl varType (Init _ (Ident varName) e) = do
  varQuadName <- getNewVariableQuadName
  let varRegister = Register varQuadName
  (v, _) <- generateQuadrupleCodeExp e
  tell [Copy varType varRegister v]
  return (addVarType varName varQuadName varType)

generateQuadrupleCodeVarDecls :: TypeQ -> [Item] -> QGM (QGMEnv -> QGMEnv)
generateQuadrupleCodeVarDecls varType [] = return id
generateQuadrupleCodeVarDecls varType (item:tail) = do
  changeEnv <- generateQuadrupleCodeVarDecl varType item
  changeEnvTail <- local changeEnv (generateQuadrupleCodeVarDecls varType tail)
  return (changeEnvTail . changeEnv) -- check order of composition


-- GENERATE QUADRUPLE CODE FOR FUNCTION ----------------------------------------

funToQuad :: ResultTypesMap -> TopDef -> Function
funToQuad funResultsMap (FnDef _ retType (Ident name) arguments (Block _ body)) = resFunction
  where
    retTypeQ = declTypeToTypeQ retType
    argumentsQ = map argToTypeName arguments
    argumentsMappingList = map (\(t, n) -> (n, (nameToArgName n, t))) argumentsQ
    argumentsMapping = Data.Map.fromList argumentsMappingList
    argumentsQReplacedNames = map (\(t, n) -> (t, nameToArgName n)) argumentsQ
    bodyQMonad = generateQuadrupleCodeStmtsList body
    startQGMEnv = QGMEnv {
      envVarTypes = argumentsMapping,
      envFunResultTypes = funResultsMap
      }
    (_, _, bodyQ) = runQGM bodyQMonad startQGMEnv startQGMState
    -- We add arg1 = arg1, ... assignments so that arguments behave like other variables (FIXME: this is a hack)
    argumentsSelfAssignments = map (\(t, n) -> Copy t (Register n) (Register n)) argumentsQReplacedNames
    bodyQWithEntryLabelAndArgSelfAss = LabelQ entryLabel : (argumentsSelfAssignments ++ bodyQ)
    resFunction = Function retTypeQ name argumentsQReplacedNames bodyQWithEntryLabelAndArgSelfAss

    argToTypeName :: Arg -> (TypeQ, String)
    argToTypeName (Arg _ argType (Ident argName)) = (declTypeToTypeQ argType, argName)

    nameToArgName :: String -> String
    nameToArgName name = "argument_" ++ name
