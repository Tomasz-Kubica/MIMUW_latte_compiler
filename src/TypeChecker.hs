-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -i./Grammar #-}

module TypeChecker ( TCMException, executeProgramCheck ) where

import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map

import AbsLatte

type TypesMap = Data.Map.Map Ident Type
type StructType = Data.Map.Map Ident Type
type StructTypes = Data.Map.Map Ident StructType
data FunctionSignature = FunctionSignature Type [Type]
type DeclaredFunctions = Data.Map.Map Ident FunctionSignature

noLoc :: BNFC'Position
noLoc = Nothing

-- Monad elements
type TCMException = String
data TCMEnv = TCMEnv {
  envTypes :: TypesMap,
  envReturnType :: Type,
  envStructTypes :: StructTypes,
  envFunctions :: DeclaredFunctions
}

type ChangeEnv = TCMEnv -> TCMEnv

type TCM a = ExceptT TCMException (ReaderT TCMEnv Identity) a

runTCM :: TCM a -> TCMEnv -> Either TCMException a
runTCM monad env = res
  where
    res = runIdentity (runReaderT (runExceptT monad) env)

-- Monad constants

printIntIdent :: Ident
printIntIdent = Ident "printInt"

printIntSignature :: FunctionSignature
printIntSignature = FunctionSignature (Void noLoc) [Int noLoc]

printStringIdent :: Ident
printStringIdent = Ident "printString"

printStringSignature :: FunctionSignature
printStringSignature = FunctionSignature (Void noLoc) [Str noLoc]

readIntIdent :: Ident
readIntIdent = Ident "readInt"

readIntSignature :: FunctionSignature
readIntSignature = FunctionSignature (Int noLoc) []

readStringIdent :: Ident
readStringIdent = Ident "readString"

readStringSignature :: FunctionSignature
readStringSignature = FunctionSignature (Str noLoc) []

standardFunctions :: DeclaredFunctions
standardFunctions = Data.Map.fromList [
  (printIntIdent, printIntSignature),
  (printStringIdent, printStringSignature),
  (readIntIdent, readIntSignature),
  (readStringIdent, readStringSignature)
  ]

emptyTCMEnv :: TCMEnv
emptyTCMEnv = TCMEnv {
  envTypes = Data.Map.empty,
  envReturnType = Void noLoc,
  envStructTypes = Data.Map.empty,
  envFunctions = standardFunctions
}

-- Access env
getType :: TCMEnv -> Ident -> Maybe Type
getType env id = Data.Map.lookup id types
  where
    types = envTypes env

getStructType :: TCMEnv -> Ident -> Maybe StructType
getStructType env id = Data.Map.lookup id structs
  where
    structs = envStructTypes env

getStructAttrType :: TCMEnv -> Ident -> Ident -> Maybe Type
getStructAttrType env struct attr = case structType of
  Nothing -> Nothing
  Just attrs -> Data.Map.lookup attr attrs
  where
    structType = getStructType env struct

getFunctionSignature :: TCMEnv -> Ident -> Maybe FunctionSignature
getFunctionSignature env id = Data.Map.lookup id functions
  where
    functions = envFunctions env

getReturnType :: TCMEnv -> Type
getReturnType = envReturnType

addVar :: Type -> Ident -> ChangeEnv
addVar t id env = env'
  where
    types = envTypes env
    types' = Data.Map.insert id t types
    env' = env {envTypes = types'}

addFun :: FunctionSignature -> Ident -> ChangeEnv
addFun signature fID env = env'
  where
    funs = envFunctions env
    funs' = Data.Map.insert fID signature funs
    env' = env {envFunctions = funs'}

setRetType :: Type -> ChangeEnv
setRetType t env = env {envReturnType = t}

-- Show location in code
showLoc :: BNFC'Position -> String
showLoc Nothing = "[Position unknown]"
showLoc (Just (line, col)) = "line " ++ sLine ++ ", colum " ++ sCol
  where
    sLine = show line
    sCol = show col

-- Compare Types
compareTypes :: Type -> Type -> Bool
compareTypes (Int _) (Int _) = True
compareTypes (Str _) (Str _) = True
compareTypes (Bool _) (Bool _) = True
compareTypes (Void _) (Void _) = True
compareTypes (Array _ t1) (Array _ t2) = compareTypes t1 t2
compareTypes (Struct _ id1) (Struct _ id2) = id1 == id2
compareTypes _ _ = False

-- Check code

-- Check if expression is valid and return it's type
checkExpr :: Expr -> TCM Type

checkExpr (EVar loc id) = do
  env <- ask
  let maybeT = getType env id
  case maybeT of
    Nothing -> throwError ("Use of variable that wasn't declared at, " ++ showLoc loc)
    Just t -> return t

checkExpr (ELitInt _loc _val) = return (Int noLoc)

checkExpr (ELitTrue _loc) = return (Bool noLoc)

checkExpr (ELitFalse _loc) = return (Bool noLoc)

checkExpr (EString _loc _val) = return (Str noLoc)

checkExpr (EStruct loc id) = do
  env <- ask
  let sType = getStructType env id
  case sType of
    Nothing -> throwError ("Invalid struct name, at " ++ showLoc loc)
    Just _ -> return (Struct noLoc id)

checkExpr (EAttr loc expr attr) = do
  expT <- checkExpr expr
  checkAttr loc expT attr

checkExpr (EApp loc id args) = do
  env <- ask
  let signature = getFunctionSignature env id
  case signature of
    Nothing -> throwError ("Trying to call undefined function, at " ++ showLoc loc)
    Just (FunctionSignature retT argsT) -> do
      compareArgs loc argsT args
      return retT

checkExpr (Neg loc expr) = do
  exprT <- checkExpr expr
  case exprT of
    Int _ -> return (Int noLoc)
    _ -> throwError ("Arithmetic negation of non integer value, at " ++ showLoc loc)

checkExpr (Not loc expr) = do
  exprT <- checkExpr expr
  case exprT of
    Bool _ -> return (Bool noLoc)
    _ -> throwError ("Logic negation of non boolean value, at " ++ showLoc loc)

checkExpr (EAnd loc expr1 expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T) of
    (Bool _, Bool _) -> return (Bool noLoc)
    _ -> throwError ("And operation of non boolean values, at " ++ showLoc loc)

checkExpr (EOr loc expr1 expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T) of
    (Bool _, Bool _) -> return (Bool noLoc)
    _ -> throwError ("Or operation of non boolean values, at " ++ showLoc loc)

checkExpr (ERel loc expr1 _relOp expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  if compareTypes expr1T expr2T
    then case (expr1T, expr2T) of
      (Bool _, Bool _) -> return (Bool noLoc)
      (Str _, Str _) -> return (Bool noLoc)
      (Int _, Int _) -> return (Bool noLoc)
      _ -> throwError ("Invalid comparison, at " ++ showLoc loc ++ ". Only values of type boolean, string or integer can be compared")
    else throwError ("Comparison of values of different type, at " ++ showLoc loc)

checkExpr (EAdd loc expr1 addOp expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T, addOp) of
    (Int _, Int _, _) -> return (Int noLoc)
    (Str _, Str _, Plus _) -> return (Str noLoc)
    _ -> throwError ("Arithmetic operation of non integer values, at " ++ showLoc loc)

checkExpr (EMul loc expr1 _mulOp expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T) of
    (Int _, Int _) -> return (Int noLoc)
    _ -> throwError ("Arithmetic operation of non integer values, at " ++ showLoc loc)

checkExpr (EArrEl loc arr index) = do
  arrT <- checkExpr arr
  indexT <- checkExpr index
  case (arrT, indexT) of
    (Array _ t, Int _) -> return t
    (Array _ t, _) -> throwError ("Array index isn't integer, at " ++ showLoc loc)
    (_, _) -> throwError ("Trying to access array element of non array value, at " ++ showLoc loc)


-- Auxiliary function for checkExpr for EAtrr
checkAttr :: BNFC'Position -> Type -> Ident -> TCM Type
checkAttr loc (Struct _ struct) attr = do
  env <- ask
  let attrType = getStructAttrType env struct attr
  case attrType of
    Nothing -> throwError ("Invalid attribute name, at " ++ showLoc loc)
    Just t -> return t
checkAttr loc _ _ = throwError ("Trying to access attribute of non structure, at " ++ showLoc loc)

-- Auxiliary function for checkExpr for EApp
compareArgs :: BNFC'Position -> [Type] -> [Expr] -> TCM ()
compareArgs loc [] [] = return ()
compareArgs loc [] (exprH:exprT) = throwError ("Too much arguments in function call, at " ++ showLoc loc)
compareArgs loc (typesH:typesT) [] = throwError ("Not enough arguments in function call, at " ++ showLoc loc)
compareArgs loc (typesH:typesT) (exprH:exprT) = do
  exprType <- checkExpr exprH
  let loc = hasPosition exprH
  if compareTypes typesH exprType
    then compareArgs loc typesT exprT
    else throwError ("Wrong argument type, at " ++ showLoc loc)

-- Stmt -> (does_contain_return, ChangeEnv)
checkStmt :: Stmt -> TCM (Bool, ChangeEnv)

checkStmt (Empty _loc) = return (False, id)

checkStmt (BStmt _loc (Block _ stmts)) = do
  (hasRet, _) <- checkStmtList stmts
  return (hasRet, id) -- Ignore env changes done by stmts inside block, they don't affect anything outside of it

checkStmt (Ass loc dest val) = do
  destT <- checkLValue loc dest
  valT <- checkExpr val
  if compareTypes destT valT
    then return (False, id)
    else throwError ("Wrong type of assigned value, at " ++ showLoc loc ++ show destT ++ " | " ++ show valT)

checkStmt (Incr loc varId) = do
  t <- checkExpr (EVar loc varId)
  case t of
    Int _ -> return (False, id)
    _ -> throwError ("Trying to increment variable of non integer type, at " ++ showLoc loc)

checkStmt (Decr loc varId) = do
  t <- checkExpr (EVar loc varId)
  case t of
    Int _ -> return (False, id)
    _ -> throwError ("Trying to decrement variable of non integer type, at " ++ showLoc loc)

checkStmt (SExp loc exp) = do
  checkExpr exp
  return (False, id)

checkStmt (Cond loc cond body) = do
  condT <- checkExpr cond
  case condT of
    Bool ma -> do
      (hasRet, _) <- checkStmt body
      return (hasRet, id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)

checkStmt (CondElse loc cond bodyT bodyF) = do
  condT <- checkExpr cond
  case condT of
    Bool ma -> do
      (hasRetT, _) <- checkStmt bodyT
      (hasRetF, _) <- checkStmt bodyF
      return (hasRetT && hasRetF, id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)

checkStmt (While loc cond body) = do
  condT <- checkExpr cond
  case condT of
    Bool ma -> do
      (hasRet, _) <- checkStmt body
      return (hasRet, id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)

checkStmt (Ret loc expr) = do
  exprT <- checkExpr expr
  env <- ask
  let expectedT = getReturnType env
  if compareTypes exprT expectedT
    then return (True, id)
    else throwError ("Invalid type of returned value, at " ++ showLoc loc)

checkStmt (VRet loc) = do
  env <- ask
  let expectedT = getReturnType env
  if compareTypes (Void noLoc) expectedT
    then return (True, id)
    else throwError ("Void return but vale was expected, at " ++ showLoc loc)

checkStmt (Decl loc declType vars) = do
  changeEnv <- declVars declType vars
  return (False, changeEnv)

-- Bool - does contain ret
checkStmtList :: [Stmt] -> TCM (Bool, ChangeEnv)
checkStmtList [] = return (False, id)
checkStmtList (h:t) = do
  (hasRetH, hChangeEnv) <- checkStmt h
  (hasRetT, tChangeEnv) <- local hChangeEnv (checkStmtList t)
  return (hasRetH || hasRetT,tChangeEnv.hChangeEnv) -- ?????

-- Check if expression is an L-value and returns it's type
checkLValue :: BNFC'Position -> Expr -> TCM Type

checkLValue _ expr@EVar {} = checkExpr expr

checkLValue _ expr@EAttr {} = checkExpr expr

checkLValue _ expr@EArrEl {} = checkExpr expr

checkLValue loc _ = throwError ("Value at left side of assignment isn't L-value, at " ++ showLoc loc)

-- Declare variable 
declVar :: Type -> Item -> TCM ChangeEnv

declVar varType (NoInit loc varID) = return (addVar varType varID)

declVar varType (Init loc varID exp) = do
  expType <- checkExpr exp
  if compareTypes expType varType
    then return (addVar varType varID)
    else throwError ("Expression of wrong type when initializing variable, at " ++ showLoc loc)

declVars :: Type -> [Item] -> TCM ChangeEnv

declVars _varType [] = return id

declVars varType (h:t) = do
  changeH <- declVar varType h
  changeT <- local changeH (declVars varType t)
  return (changeT.changeH)

checkTopDef :: TopDef -> TCM ()
checkTopDef (FnDef loc retType funID args body) = do
  (argsT, insArgs) <- insertArgs args
  let signature = FunctionSignature retType argsT
  let insRetT = setRetType retType
  let insAll = insArgs.insRetT
  (isRet, _) <- local insAll (checkStmt (BStmt noLoc body))
  let isRetVoid = compareTypes retType (Void noLoc)
  if isRetVoid || isRet
    then return ()
    else throwError ("Function with non void return type can possibly end without return statement, at " ++ showLoc loc)


insertArgs :: [Arg] -> TCM ([Type], ChangeEnv)

insertArgs [] = return ([], id)

insertArgs ((Arg loc argT argID):t) = do
  let insArg = addVar argT argID
  (tailT, insT) <- local insArg (insertArgs t)
  return (argT:tailT, insT.insArg)

checkTopDefs :: [TopDef] -> TCM ()

checkTopDefs defs = do
  -- TODO: Maybe check if function names are unique
  let inserts = map insertFunFromDef defs
  let joinedInsert = foldr (.) id inserts
  let checks = map checkTopDef defs
  let checkAll = sequence_ checks
  local joinedInsert checkAll

insertFunFromDef :: TopDef -> TCMEnv -> TCMEnv
insertFunFromDef (FnDef loc retType funID args _body) = addFun signature funID
  where
    signature = FunctionSignature retType argsT
    argsT = map argToType args
    argToType (Arg _loc argType _id) = argType

checkProg :: Program -> TCM ()
checkProg (Program _ topDefs) = do
  checkTopDefs topDefs

executeProgramCheck :: Program -> Either TCMException ()
executeProgramCheck program = runTCM checkMonad emptyTCMEnv
  where
    checkMonad = checkProg program
