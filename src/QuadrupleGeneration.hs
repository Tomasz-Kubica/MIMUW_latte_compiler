{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module QuadrupleGeneration (topDefsToQuad, declTypeToTypeQ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map
import Data.Sort

import AbsLatte
import LatteStdLibFunctions
import QuadrupleCode
import SimplifyStmt
import RemoveDeadQuadruples

-- Monad -----------------------------------------------------------------------

-- Maps variables to their names in quadruple code and their types
type TypesMap = Data.Map.Map String (String, TypeQ)

type ResultTypesMap = Data.Map.Map String TypeQ

-- Structs attributes and methods
data StructInfo = StructInfo {
  structInfoName :: String,
  structAttributes :: Data.Map.Map Ident (TypeQ, Integer),
  structMethods :: Data.Map.Map Ident (TypeQ, Integer, String)
}

type StructsMap = Data.Map.Map String StructInfo

data QGMEnv = QGMEnv {
  envVarTypes :: TypesMap,
  envFunResultTypes :: ResultTypesMap,
  functionName :: String, -- Used to generate unique global string literals names
  structsInfo :: StructsMap,
  envStructAttributes :: Data.Map.Map Ident (TypeQ, Integer) -- name -> (type, index)
}

data QGMState = QGMState {
  stateIdCounter :: Integer,
  stringLiterals :: [(String, String)] -- (name, value)
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
  stateIdCounter = firstFreeLabel,
  stringLiterals = []
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
      unwrapMaybe Nothing = error ("getVarQuadNameAndType: variable not found, varName = " ++ varName)

addStringConstant :: String -> String -> QGM ()
addStringConstant name value = do
  state <- get
  let stringLiteralsList = stringLiterals state
  let newStringLiteralsList = (name, value) : stringLiteralsList
  put (state { stringLiterals = newStringLiteralsList })

getCurrentFunctionName :: QGM String
getCurrentFunctionName = asks functionName

getAttrInfo :: String -> Ident -> QGM (TypeQ, Integer)
getAttrInfo structName attrName = do
  env <- ask
  let structsMap = structsInfo env
  let structInfo = structsMap Data.Map.! structName
  let attributesMap = structAttributes structInfo
  let maybeAttrInfo = Data.Map.lookup attrName attributesMap
  let attrInfo = unwrapMaybe maybeAttrInfo
  return attrInfo
    where
      unwrapMaybe (Just x) = x
      unwrapMaybe Nothing = error "getAttrInfo: attribute not found"

getMethodInfo :: String -> Ident -> QGM (TypeQ, Integer, String)
getMethodInfo structName methodName = do
  env <- ask
  let structsMap = structsInfo env
  let structInfo = structsMap Data.Map.! structName
  let methodsMap = structMethods structInfo
  let maybeMethodInfo = Data.Map.lookup methodName methodsMap
  let methodInfo = unwrapMaybe maybeMethodInfo
  return methodInfo
    where
      unwrapMaybe (Just x) = x
      unwrapMaybe Nothing = error "getMethodInfo: method not found"

-- Quadruple generation --------------------------------------------------------

generateQuadrupleCodeExp :: Expr -> QGM (Value, TypeQ)

-- Nothing to do just return variable as value
generateQuadrupleCodeExp (EVar _ (Ident varName)) = do
  -- Check if this is var or attribute accessed inside method
  env <- ask
  case Data.Map.lookup (Ident varName) (envStructAttributes env) of
    Nothing -> do -- this is just a variable
      -- error (show (envStructAttributes env))
      (varQuadName, varType) <- getVarQuadNameAndType varName
      return (Register varQuadName, varType)
    Just (varType, idx) -> do -- this is attribute accessed inside method
      generateQuadrupleCodeExp (EAttr Nothing (EVar Nothing (Ident "self")) (Ident varName))

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

generateQuadrupleCodeExp (Neg _ x) = generateQuadrupleCodeExp (EMul Nothing minusOne operation x)
  where
    minusOne = ELitInt Nothing (-1)
    operation = Times Nothing

-- String literals

generateQuadrupleCodeExp (EString _ s) = do
  tmpRegisterName <- getTmpRegisterName
  let Register registerName = tmpRegisterName
  functionName <- getCurrentFunctionName
  let constantName = "string_" ++ registerName ++ "_" ++ functionName
  let stringLength = toInteger (length s) + 1 -- +1 for null terminator
  addStringConstant constantName s
  tell [ConstString tmpRegisterName constantName stringLength]
  return (tmpRegisterName, StringQ)

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

-- Objects

generateQuadrupleCodeExp (EAttr _ expr name) = do
  (structV, structT) <- generateQuadrupleCodeExp expr
  case structT of
    StructQ className -> do
      tmpRegisterName <- getTmpRegisterName
      (attrType, idx) <- getAttrInfo className name
      tell [GetAttr attrType tmpRegisterName className structV idx]
      return (tmpRegisterName, attrType)
    _ -> error "generateQuadrupleCodeExp: accessing attribute of none struct"

generateQuadrupleCodeExp (EStruct _ (Ident name)) = do
  tmpRegisterName <- getTmpRegisterName
  tell [NewStruct tmpRegisterName name] -- Generating LLVM for this will handle setting method table pointer
  return (tmpRegisterName, StructQ name)

generateQuadrupleCodeExp (ENull _ (Ident name)) = do
  let structT = StructQ name
  tmpRegisterName <- getTmpRegisterName
  tell [Copy structT tmpRegisterName (ConstNull name)]
  return (tmpRegisterName, structT)

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

generateQuadrupleCodeCond otherExp trueL falseL = do
  (v, _) <- generateQuadrupleCodeExp otherExp
  tell [ ConditionalJump v trueL falseL ]

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
declTypeToTypeQ (Str _) = StringQ
declTypeToTypeQ (Struct _ (Ident name)) = StructQ name

generateQuadrupleCodeStmt :: Stmt -> QGM (QGMEnv -> QGMEnv)

generateQuadrupleCodeStmt (Empty _) = return id

generateQuadrupleCodeStmt (BStmt _ (Block _ block)) = do
  generateQuadrupleCodeStmtsList block
  return id

generateQuadrupleCodeStmt (Decl _ declType items) = do
  let typeQ = declTypeToTypeQ declType
  generateQuadrupleCodeVarDecls typeQ items

-- Special case for assigning to struct attribute
generateQuadrupleCodeStmt (Ass _ (EAttr _ struct attrName) source) = do
  (sourceV, sourceT) <- generateQuadrupleCodeExp source
  (structV, structT) <- generateQuadrupleCodeExp struct
  tmpRegister <- getTmpRegisterName
  case structT of
    StructQ structName -> do
      (_attrType, idx) <- getAttrInfo structName attrName
      tell [SetAttr structName structV idx sourceT sourceV tmpRegister]
      return id
    _ -> error "generateQuadrupleCodeStmt: Assigning to attribute of none struct"


generateQuadrupleCodeStmt (Ass _ (EVar _ (Ident varName)) source) = do
  env <- ask
  case Data.Map.lookup (Ident varName) (envStructAttributes env) of
    Nothing -> do -- this is just a variable
      (v, t) <- generateQuadrupleCodeExp source
      (quadName, _) <- getVarQuadNameAndType varName
      tell [Copy t (Register quadName) v]
      return id
    Just (varType, idx) -> do -- this is attribute access from method
      let selfVar = EVar Nothing (Ident "self")
      generateQuadrupleCodeStmt (Ass Nothing (EAttr Nothing selfVar (Ident varName)) source)

-- -- Special case for assigning to struct attribute
-- generateQuadrupleCodeStmt (Incr _ (EAttr _ struct attrName)) = do
--   (structV, structT) <- generateQuadrupleCodeExp struct
--   loadedAttr <- getTmpRegisterName
--   incrementedAttr <- getTmpRegisterName
--   case structT of 
--     StructQ structName -> do
--       let idx = 1 -- TODO: get attribute index from name
--       tell [
--         GetAttr IntQ loadedAttr structName structV idx,
--         ArithmeticOperation incrementedAttr IntQ loadedAttr Add (ConstInt 1)
--         SetAttr structName structV idx IntQ incrementedAttr
--         ]
--       return id
--     _ -> error "generateQuadrupleCodeStmt: Incrementing attribute of none struct"

generateQuadrupleCodeStmt (Incr _ (Ident varName)) = do
  let var = EVar Nothing (Ident varName)
  let const = ELitInt Nothing 1
  let result = EAdd Nothing var (Plus Nothing) const
  generateQuadrupleCodeStmt (Ass Nothing var result)
  return id

-- -- Special case for assigning to struct attribute
-- generateQuadrupleCodeStmt (Decr _ (EAttr _ struct attrName)) = do
--   (structV, structT) <- generateQuadrupleCodeExp struct
--   loadedAttr <- getTmpRegisterName
--   decrementedAttr <- getTmpRegisterName
--   case structT of 
--     StructQ structName -> do
--       let idx = 1 -- TODO: get attribute index from name
--       tell [
--         GetAttr IntQ loadedAttr structName structV idx,
--         ArithmeticOperation decrementedAttr IntQ loadedAttr Sub (ConstInt 1)
--         SetAttr structName structV idx IntQ decrementedAttr
--         ]
--       return id
--     _ -> error "generateQuadrupleCodeStmt: Incrementing attribute of none struct"

generateQuadrupleCodeStmt (Decr _ (Ident varName)) = do
  let var = EVar Nothing (Ident varName)
  let const = ELitInt Nothing 1
  let result = EAdd Nothing var (Minus Nothing) const
  generateQuadrupleCodeStmt (Ass Nothing var result)
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


generateQuadrupleCodeStmtsList :: [Stmt] -> QGM ()
generateQuadrupleCodeStmtsList stmts = generateQuadrupleCodeStmtsListPostSimplify stmts''
  where
    stmts' = map simplifyStmt stmts
    stmts'' = removeDeadStmts stmts'

-- Function for generating code for statements lists
generateQuadrupleCodeStmtsListPostSimplify :: [Stmt] -> QGM ()
generateQuadrupleCodeStmtsListPostSimplify [] = return ()
generateQuadrupleCodeStmtsListPostSimplify (stmt:tail) = do
  let simplifiedStmt = simplifyStmt stmt
  changeEnv <- generateQuadrupleCodeStmt simplifiedStmt
  local changeEnv (generateQuadrupleCodeStmtsListPostSimplify tail)

-- Function for generating code for variable declarations
generateQuadrupleCodeVarDecl :: TypeQ -> Item -> QGM (QGMEnv -> QGMEnv)

-- Special case for struct, should be initialize with new struct
generateQuadrupleCodeVarDecl (StructQ structName) (NoInit _ (Ident varName)) = do
  varQuadName <- getNewVariableQuadName
  let varRegister = Register varQuadName
  tell [NewStruct varRegister structName]
  return (addVarType varName varQuadName (StructQ structName))


-- Special case for string, should be initialize with "" (empty string)
generateQuadrupleCodeVarDecl StringQ (NoInit _ (Ident varName)) = do
  varQuadName <- getNewVariableQuadName
  let varRegister = Register varQuadName
  (stringLoc, _) <- generateQuadrupleCodeExp (EString Nothing "")
  tell [ Copy StringQ varRegister stringLoc]
  return (addVarType varName varQuadName StringQ)

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

-- GENERATE QUADRUPLE CODE FOR TOP DEFINITIONS ---------------------------------

topDefsToQuad :: [TopDef] -> ([Structure], [Function])
topDefsToQuad topDefs = (structures, allFunctions)
  where
    funDefs = filter isFunDef topDefs
    classDefs = filter isClassDef topDefs

    resultTypesMap = genFunResultMap funDefs
    (structMap, methods) = processClassDefs classDefs
    
    env = QGMEnv {
      -- envVarTypes = Data.Map.empty,
      envFunResultTypes = resultTypesMap,
      -- functionName = "",
      structsInfo = structMap,
      envStructAttributes = Data.Map.empty
    }
    functions = map (funToQuad env) funDefs

    (structures, methodFunctions) = classesToQuad resultTypesMap structMap methods

    allFunctions = methodFunctions ++ functions

    isFunDef :: TopDef -> Bool
    isFunDef FnDef {} = True
    isFunDef _ = False
    isClassDef :: TopDef -> Bool
    isClassDef ClassDef {} = True
    isClassDef _ = False

    genFunResultMap :: [TopDef] -> Data.Map.Map String TypeQ
    genFunResultMap topDefs = funResultsMapWithStdLib
      where
        nameTypeList = map (\(FnDef _ t (Ident name) _ _) -> (name, declTypeToTypeQ t)) topDefs
        funResultsMap = Data.Map.fromList nameTypeList
        funResultsMapWithStdLib = Data.Map.union funResultsMap stdFunctionsResultTypes


-- GENERATE QUADRUPLE CODE FOR FUNCTION ----------------------------------------

funToQuad :: QGMEnv -> TopDef -> Function
funToQuad env (FnDef _ retType (Ident name) arguments (Block _ body)) = resFunction
  where
    retTypeQ = declTypeToTypeQ retType
    argumentsQ = map argToTypeName arguments
    argumentsMappingList = map (\(t, n) -> (n, (nameToArgName n, t))) argumentsQ
    argumentsMapping = Data.Map.fromList argumentsMappingList
    argumentsQReplacedNames = map (\(t, n) -> (t, nameToArgName n)) argumentsQ
    bodyQMonad = generateQuadrupleCodeStmtsList body
    startQGMEnv = env {
      envVarTypes = argumentsMapping,
      functionName = name
      }
    (_, endState, bodyQ) = runQGM bodyQMonad startQGMEnv startQGMState
    bodyQNoDead = removeDeadQuadruples bodyQ
    -- We add arg1 = arg1, ... assignments so that arguments behave like other variables (FIXME: this is a hack)
    argumentsSelfAssignments = map (\(t, n) -> Copy t (Register n) (Register n)) argumentsQReplacedNames
    bodyQWithEntryLabelAndArgSelfAss = LabelQ entryLabel : (argumentsSelfAssignments ++ bodyQNoDead)
    -- If function return type is void, we add return void at the end of function
    returnVoid = [ReturnVoid | retTypeQ == VoidQ]
    finalBody = bodyQWithEntryLabelAndArgSelfAss ++ returnVoid
    constDeclarations = stringLiterals endState
    resFunction = Function retTypeQ name argumentsQReplacedNames finalBody constDeclarations

    argToTypeName :: Arg -> (TypeQ, String)
    argToTypeName (Arg _ argType (Ident argName)) = (declTypeToTypeQ argType, argName)

    nameToArgName :: String -> String
    nameToArgName name = "argument_" ++ name

-- GENERATE QUADRUPLE CODE FOR STRUCTURE DEFINITION ----------------------------

classesToQuad :: ResultTypesMap -> StructsMap -> [(String, [TopDef])] -> ([Structure], [Function])
classesToQuad resultTypeMap structMap methodsLists = (structures, methods)
  where
    structures = map structInfoToStructure (Data.Map.elems structMap)
    methods = concatMap (\(structName, methods) -> map (methodDefinitionToFunction structName) methods) methodsLists

    structInfoToStructure :: StructInfo -> Structure
    structInfoToStructure structInfo = Structure name attributesList methodsList
      where
        -- Name of structure
        name = structInfoName structInfo
        -- Attributes of structure
        attributesMap = structAttributes structInfo
        attributesListWithKeys = Data.Map.toList attributesMap
        attributesListNoKeys = map snd attributesListWithKeys -- Take only values (second element of tuple)
        attributesListSorted = sortOn snd attributesListNoKeys -- Sort by index (second element of tuple)
        attributesList = map fst attributesListSorted -- Take only types (first element of tuple)
        -- Names of methods for methods table
        methodsMap = structMethods structInfo
        methodsListWithKeys = Data.Map.toList methodsMap
        methodsListNoKeys = map (\(_k, (_retT, idx, name)) -> (idx, name)) methodsListWithKeys -- Take only idx and name
        methodsListSorted = sortOn fst methodsListNoKeys -- Sort by index
        methodsList = map snd methodsListSorted -- Take only names

    methodDefinitionToFunction :: String -> TopDef -> Function
    methodDefinitionToFunction structName fnDef = funToQuad env fnDef
      where
        attributes = structAttributes (structMap Data.Map.! structName)
        env = QGMEnv {
          -- envVarTypes will be set by funToQuad
          envFunResultTypes = resultTypeMap,
          -- functionName will be set by funToQuad
          structsInfo = structMap,
          envStructAttributes = attributes
        }


-- Converts list of class definitions to structs info and list of methods (functions) to generate
processClassDefs :: [TopDef] -> (StructsMap, [(String, [TopDef])])
processClassDefs = foldl foldAux (Data.Map.empty, [])
  where
    foldAux (structMap, methodsList) classDef = (structMap', methodsList')
      where
        (structMap', methods) = processClassDef structMap classDef
        methodsList' = methods : methodsList

processClassDef :: StructsMap -> TopDef -> (StructsMap, (String, [TopDef]))
processClassDef structMap classDef = (structMap', (className, methods))
  where
    (structInfo, (className, methods)) = structToInfo structMap classDef
    structMap' = Data.Map.insert className structInfo structMap

-- Converts class definition to struct info and list of methods (functions) to generate
structToInfo :: StructsMap -> TopDef -> (StructInfo, (String, [TopDef]))
structToInfo _ (ClassDef _ (Base _ (Ident name) members)) = (info', methods')
  where
    info = StructInfo {
      structInfoName = name,
      structAttributes = Data.Map.empty,
      structMethods = Data.Map.empty
    }
    (modInfo, methods) = membersToInfo members name
    info' = modInfo info
    methods' = (name, methods)
structToInfo known_structs (ClassDef _ (Extends _ (Ident name) (Ident parent_name) members)) = (info'', methods')
  where
    info = known_structs Data.Map.! parent_name
    info' = info { structInfoName = name }
    (modInfo, methods) = membersToInfo members name
    info'' = modInfo info'
    methods' = (name, methods)


membersToInfo :: [ClassMember] -> String -> (StructInfo -> StructInfo, [TopDef])
membersToInfo [] _ = (id, [])
membersToInfo (member:tail) className = (tailModInfo . modInfo, methods ++ tailMethods)
  where
   (modInfo, methods) = memberToInfo member className
   (tailModInfo, tailMethods) = membersToInfo tail className

memberToInfo :: ClassMember -> String -> (StructInfo -> StructInfo, [TopDef])
memberToInfo (Attr _ attrType attrName) className = (modInfo, [])
  where
    modInfo info = info'
      where
        typeQ = declTypeToTypeQ attrType
        attrMap = structAttributes info
        attrNumber = toInteger (Data.Map.size attrMap + attrNumberOffset)
        attrMap' = Data.Map.insert attrName (typeQ, attrNumber) attrMap
        info' = info { structAttributes = attrMap' }
memberToInfo (Method _ retT (Ident methodName) arguments code) className = (modInfo, [method])
  where
    methodFunctionName = className ++ "_method_" ++ methodName
    selfArg = Arg Nothing (Struct Nothing (Ident className)) (Ident "self")
    arguments' = selfArg:arguments
    method = FnDef Nothing retT (Ident methodFunctionName) arguments' code
    modInfo info = info'
      where
        typeQ = declTypeToTypeQ retT
        methodMap = structMethods info
        methodNumber = case Data.Map.lookup (Ident methodName) methodMap of
          Nothing -> toInteger (Data.Map.size methodMap + 1) -- New method
          Just (_, n, _) -> n -- Method already exists in parent class and has a number in method table
        methodMap' = Data.Map.insert (Ident methodName) (typeQ, methodNumber, methodFunctionName) methodMap
        info' = info { structMethods = methodMap' }

attrNumberOffset :: Int
-- attrNumberOffset = 1 -- 0 is reserved for method table pointer TODO: uncomment this after implementing method table pointer
attrNumberOffset = 0 -- method table pointer is not implemented yet
