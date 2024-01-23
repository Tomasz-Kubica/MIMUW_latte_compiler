-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeChecker ( TCMException, executeProgramCheck ) where

import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad (when, unless, foldM)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map
import qualified Data.Set
import Data.List (nub)
import Data.Maybe (isJust)

import AbsLatte
import SimplifyExp

data FunctionSignature = FunctionSignature Type [Type]

data StructType = StructType {
  structTypeAttrs :: Data.Map.Map Ident Type,
  structTypeMethods :: Data.Map.Map Ident FunctionSignature,
  structTypeParent :: Maybe Ident
}


type TypesMap = Data.Map.Map Ident Type
type StructTypes = Data.Map.Map Ident StructType
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

data TCMState = TCMState {
  stateIdCounter :: Integer
}

type TCMWrite = [Int]

type TCM a = ExceptT TCMException (ReaderT TCMEnv (WriterT TCMWrite (StateT TCMState Identity))) a

runTCM :: TCM a -> TCMEnv -> TCMState -> (Either TCMException a, TCMState, TCMWrite)
runTCM monad env state = (res, state', written)
  where
    ((res, written), state') = runIdentity (runStateT (runWriterT (runReaderT (runExceptT monad) env)) state)

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

emptyTCMState :: TCMState
emptyTCMState = TCMState {
  stateIdCounter = 0
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
  Just structT -> Data.Map.lookup attr (structTypeAttrs structT)
  where
    structType = getStructType env struct

getStructMethod :: TCMEnv -> Ident -> Ident -> Maybe FunctionSignature
getStructMethod env struct method = case structType of
  Nothing -> Nothing
  Just structT -> Data.Map.lookup method (structTypeMethods structT)
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

-- Access state
getNewId :: TCM Integer
getNewId = do
  state <- get
  let id = stateIdCounter state
  put (state { stateIdCounter = id + 1 })
  return id

getTmpRegisterName :: TCM String
getTmpRegisterName = do
  id <- getNewId
  return ("temp_" ++ show id)

-- Show location in code
showLoc :: BNFC'Position -> String
showLoc Nothing = "[Position unknown]"
showLoc (Just (line, col)) = "line " ++ sLine ++ ", colum " ++ sCol
  where
    sLine = show line
    sCol = show col

-- Check for duplicates
noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = length xs == length (nub xs)

-- Compare Types
compareTypes :: Type -> Type -> TCM Bool
compareTypes (Int _) (Int _) = return True
compareTypes (Str _) (Str _) = return True
compareTypes (Bool _) (Bool _) = return True
compareTypes (Void _) (Void _) = return True
compareTypes (Array _ t1) (Array _ t2) = error "arrays are not implemented" -- compareTypes t1 t2
compareTypes (Struct _ id1) (Struct _ id2) = isParentClass id1 id2
compareTypes _ _ = return False

isParentClass :: Ident -> Ident -> TCM Bool
isParentClass child parent = do
  if child == parent
    then return True
    else do
      env <- ask
      let structs = envStructTypes env
      let childStruct = structs Data.Map.! child
      case structTypeParent childStruct of
        Nothing -> return False
        Just parentName -> isParentClass parentName parent

checkType :: Type -> TCM ()
checkType (Struct loc id) = do
  env <- ask
  let structType = getStructType env id
  case structType of
    Nothing -> throwError ("Unknown type, at " ++ showLoc loc)
    Just _ -> return ()
-- Basic types are ok
checkType _ = return ()

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

checkExpr (EStruct loc className) = do
  env <- ask
  let sType = getStructType env className
  case sType of
    Nothing -> throwError ("Invalid struct name, at " ++ showLoc loc)
    Just _ -> return (Struct noLoc className)

checkExpr (ENull loc className) = do
  env <- ask
  let sType = getStructType env className
  case sType of
    Nothing -> throwError ("Invalid struct name, at " ++ showLoc loc)
    Just _ -> return (Struct noLoc className)

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

checkExpr (EMethod loc expr name args) = do
  expT <- checkExpr expr
  case expT of
    Struct _ className -> do
      env <- ask
      let method = getStructMethod env className name
      case method of
        Nothing -> throwError ("Trying to call undefined method, at " ++ showLoc loc)
        Just (FunctionSignature retT argsT) -> do
          compareArgs loc argsT args
          return retT
    _ -> throwError ("Trying to call method of non structure, at " ++ showLoc loc)

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
  comp <- compareTypes expr1T expr2T
  if comp
    then case (expr1T, expr2T) of
      (Bool _, Bool _) -> return (Bool noLoc)
      (Str _, Str _) -> return (Bool noLoc)
      (Int _, Int _) -> return (Bool noLoc)
      (Struct _ _, Struct _ _) -> return (Bool noLoc)
      _ -> throwError ("Invalid comparison, at " ++ showLoc loc ++ ". Only values of type boolean, string, integer or structs can be compared")
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
compareArgs loc [] (exprH:exprT) = throwError ("Too many arguments in function call, at " ++ showLoc loc)
compareArgs loc (typesH:typesT) [] = throwError ("Not enough arguments in function call, at " ++ showLoc loc)
compareArgs loc (typesH:typesT) (exprH:exprT) = do
  exprType <- checkExpr exprH
  let loc = hasPosition exprH
  comp <- compareTypes exprType typesH
  if comp
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
  comp <- compareTypes valT destT
  if comp
    then return (False, id)
    else throwError ("Wrong type of assigned value, at " ++ showLoc loc) -- ++ show destT ++ " | " ++ show valT)

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
      let simpCond = simplifyExp cond
      let isCertain = isTrue simpCond
      return (hasRet && isCertain, id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)
    where
      isTrue (ELitTrue _) = True
      isTrue _ = False

checkStmt (CondElse loc cond bodyT bodyF) = do
  condT <- checkExpr cond
  case condT of
    Bool ma -> do
      (hasRetT, _) <- checkStmt bodyT
      (hasRetF, _) <- checkStmt bodyF
      let simpCond = simplifyExp cond
      let (ignoreT, ignoreF) = condToIgnore simpCond
      return ((hasRetT || ignoreT) && (hasRetF || ignoreF), id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)
    where
      condToIgnore (ELitTrue _) = (False, True)
      condToIgnore (ELitFalse _) = (True, False)
      condToIgnore _ = (False, False)

checkStmt (While loc cond body) = do
  condT <- checkExpr cond
  case condT of
    Bool ma -> do
      (hasRet, _) <- checkStmt body
      let simpCond = simplifyExp cond
      let isCertain = isTrue simpCond
      return (hasRet && isCertain, id)
    _ -> throwError ("Condition of non boolean type, at " ++ showLoc loc)
    where
      isTrue (ELitTrue _) = True
      isTrue _ = False

checkStmt (Ret loc expr) = do
  exprT <- checkExpr expr
  env <- ask
  let expectedT = getReturnType env
  comp <- compareTypes exprT expectedT
  if comp
    then return (True, id)
    else throwError ("Invalid type of returned value, at " ++ showLoc loc)

checkStmt (VRet loc) = do
  env <- ask
  let expectedT = getReturnType env
  comp <- compareTypes (Void noLoc) expectedT
  if comp
    then return (True, id)
    else throwError ("Void return but vale was expected, at " ++ showLoc loc)

checkStmt (Decl loc declType vars) = do
  checkType declType
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

declVar varType (NoInit loc varID) = do
  env <- ask
  let lookupResult = getType env varID
  let doesExist = isJust lookupResult
  when doesExist $ throwError ("Declared variable of non unique name (variable's name must be unique inside it's scope), at " ++ showLoc loc)
  return (addVar varType varID)

declVar varType (Init loc varID exp) = do
  expType <- checkExpr exp
  comp <- compareTypes expType varType
  if comp
    then return (addVar varType varID)
    else throwError ("Expression of wrong type when initializing variable, at " ++ showLoc loc)

declVars :: Type -> [Item] -> TCM ChangeEnv

declVars _varType [] = return id

declVars varType (h:t) = do
  changeH <- declVar varType h
  changeT <- local changeH (declVars varType t)
  return (changeT.changeH)

checkTopDef :: TopDef -> TCM ()

checkTopDef (FnDef loc retType _ args body) = do
  (argsT, argsID, insArgs) <- insertArgs args
  unless (noDuplicates argsID) $ throwError ("Non unique arguments names, at " ++ showLoc loc)
  -- let signature = FunctionSignature retType argsT
  let insRetT = setRetType retType
  let insAll = insArgs.insRetT
  (isRet, _) <- local insAll (checkStmt (BStmt noLoc body))
  isRetVoid <- compareTypes (Void noLoc) retType
  if isRetVoid || isRet
    then return ()
    else throwError ("Function with non void return type can possibly end without return statement, at " ++ showLoc loc)

checkTopDef (ClassDef loc (Base _ className members)) = checkClassTopDef className members

checkTopDef (ClassDef loc (Extends _ className _ members)) = checkClassTopDef className members

checkClassTopDef :: Ident -> [ClassMember] -> TCM ()
checkClassTopDef className members = do
  let methods = filter isMethod members
  env <- ask
  let classDefinition = envStructTypes env Data.Map.! className
  let attrs = structTypeAttrs classDefinition
  local (insertAttrs attrs . insertSelf) (mapM_ checkMethod methods)
  where
    isMethod :: ClassMember -> Bool
    isMethod Method {} = True
    isMethod _ = False

    insertSelf :: (TCMEnv -> TCMEnv)
    insertSelf = addVar (Struct noLoc className) (Ident "self")

    insertAttrs :: Data.Map.Map Ident Type -> (TCMEnv -> TCMEnv)
    insertAttrs attrs = foldr ((.) . insertAttr) id (Data.Map.toList attrs)

    insertAttr :: (Ident, Type) -> (TCMEnv -> TCMEnv)
    insertAttr (id, t) env = env'
      where
        types = envTypes env
        types' = Data.Map.insert id t types
        env' = env {envTypes = types'}

    checkMethod :: ClassMember -> TCM ()
    checkMethod (Method loc retT name args block) = do
      let functionDef = FnDef loc retT name args block
      checkTopDef functionDef

insertArgs :: [Arg] -> TCM ([Type], [Ident],ChangeEnv)

insertArgs [] = return ([], [], id)

insertArgs ((Arg loc argT argID):t) = do
  let insArg = addVar argT argID
  (tailT, tailID, insT) <- local insArg (insertArgs t)
  return (argT:tailT, argID:tailID, insT.insArg)

checkTopDefs :: [TopDef] -> TCM ()

checkTopDefs defs = do
  let (funDefs, classDefs) = splitTopDefs defs

  -- Insert classes to env
  let classNames = map classToName classDefs
  let knownClasses = Data.Set.fromList classNames
  classInsert <- insertClassFromDefs knownClasses classDefs

  -- Insert functions signatures to env
  funInserts <- local classInsert (mapM insertFunFromDef funDefs)
  let joinedFunInsert = foldr (.) id funInserts

  let insertFunAndClass = joinedFunInsert . classInsert
  -- After inserting functions signatures and classes we perform type checking
  -- of functions and methods implementations
  let checkAll = mapM_ checkTopDef defs
  local insertFunAndClass checkAll

-- Split top definitions into function and class definitions
splitTopDefs :: [TopDef] -> ([TopDef], [TopDef])
splitTopDefs defs = (fnDefs, classDefs)
  where
    fnDefs = filter isFnDef defs
    classDefs = filter isClassDef defs

    isFnDef FnDef {} = True
    isFnDef _ = False

    isClassDef ClassDef {} = True
    isClassDef _ = False

classToName :: TopDef -> Ident
classToName (ClassDef _ (Base _ name _)) = name
classToName (ClassDef _ (Extends _ name _ _)) = name

insertFunFromDef :: TopDef -> TCM (TCMEnv -> TCMEnv)
insertFunFromDef (FnDef loc retType funID args _body) = do
  -- Check return type
  checkType retType
  -- Check arguments types
  mapM_ checkType argsT
  return (addFun signature funID)
  where
    signature = FunctionSignature retType argsT
    argsT = map argToType args
    argToType (Arg _loc argType _id) = argType

insertFunFromDef _ = error "insertFunFromDef called with non function definition"

insertClassFromDefs :: Data.Set.Set Ident -> [TopDef] -> TCM (TCMEnv -> TCMEnv)
insertClassFromDefs _ [] = return id
insertClassFromDefs knownClasses (h:t) = do
  insertH <- insertClassFromDef knownClasses h
  insertT <- local insertH (insertClassFromDefs knownClasses t)
  return (insertT.insertH)

insertClassFromDef :: Data.Set.Set Ident -> TopDef -> TCM (TCMEnv -> TCMEnv)

insertClassFromDef knownClasses (ClassDef loc (Base _ className classMembers)) = do
  env <- ask
  let structs = envStructTypes env
  case Data.Map.lookup className structs of
    Just _ -> throwError ("Class with non unique name, at " ++ showLoc loc)
    Nothing -> do
      -- No parent class, start with fresh definition
      let structType = StructType Data.Map.empty Data.Map.empty Nothing
      structType' <- addClassMembers knownClasses structType classMembers
      let structs' = Data.Map.insert className structType' structs
      return (\env -> env {envStructTypes = structs'})

insertClassFromDef knownClasses (ClassDef loc (Extends _ className baseClass classMembers)) = do
  env <- ask
  let structs = envStructTypes env
  case Data.Map.lookup className structs of
    Just _ -> throwError ("Class with non unique name, at " ++ showLoc loc)
    Nothing -> case Data.Map.lookup baseClass structs of
      Nothing -> throwError ("Class with non existing parent class, at " ++ showLoc loc)
      Just StructType { structTypeAttrs = attrs, structTypeMethods = methods } -> do
        -- Start with base class definition
        let structType = StructType attrs methods (Just baseClass)
        structType' <- addClassMembers knownClasses structType classMembers
        let structs' = Data.Map.insert className structType' structs
        return (\env -> env {envStructTypes = structs'})

insertClassFromDef _ _ = error "insertClassFromDef called with non class definition"

addClassMembers :: Data.Set.Set Ident -> StructType -> [ClassMember] -> TCM StructType
addClassMembers knownClasses = foldM (addClassMember knownClasses)

addClassMember :: Data.Set.Set Ident -> StructType -> ClassMember -> TCM StructType

addClassMember knownClasses structType (Attr loc fieldType fieldID) = do
  -- Check attribute type
  if isTypeDefined fieldType
    then return ()
    else throwError ("Unknown type, at " ++ showLoc loc)

  let attrs = structTypeAttrs structType
  if Data.Map.member fieldID attrs
    then
      throwError ("Attribute with non unique name, at " ++ showLoc loc)
    else do
      let attrs' = Data.Map.insert fieldID fieldType attrs
      return (structType { structTypeAttrs = attrs' })
  where
    isTypeDefined :: Type -> Bool
    isTypeDefined (Struct _ id) = Data.Set.member id knownClasses
    isTypeDefined _ = True

addClassMember knownClasses structType (Method loc retT methodName args _block) = do
  let argsT = map argToType args

  -- Check return type and arguments types
  if isTypeDefined retT
    then return ()
    else throwError ("Unknown type, at " ++ showLoc loc)
  let correctArgs = all isTypeDefined argsT
  if correctArgs
    then return ()
    else throwError ("Unknown type, at " ++ showLoc loc)

  let methods = structTypeMethods structType
  let method = FunctionSignature retT argsT
  case Data.Map.lookup methodName methods of
    Just oldSignature -> if compareSignatures oldSignature method
      then return structType
      else throwError ("Method replacing method from parent class has to have the same return type and argument types, at " ++ showLoc loc)
    Nothing -> do
      let methods' = Data.Map.insert methodName method methods
      return (structType { structTypeMethods = methods' })
  where
    argToType (Arg _loc argType _id) = argType

    isTypeDefined :: Type -> Bool
    isTypeDefined (Struct _ id) = Data.Set.member id knownClasses
    isTypeDefined _ = True

    exactCompareTypes :: Type -> Type -> Bool
    exactCompareTypes (Struct _ id1) (Struct _ id2) = id1 == id2
    exactCompareTypes (Int _) (Int _) = True
    exactCompareTypes (Str _) (Str _) = True
    exactCompareTypes (Bool _) (Bool _) = True
    exactCompareTypes (Void _) (Void _) = True
    exactCompareTypes _ _ = False

    compareSignatures :: FunctionSignature -> FunctionSignature -> Bool
    compareSignatures (FunctionSignature ret1 args1) (FunctionSignature ret2 args2) = retSame && argsSame
      where
        retSame = exactCompareTypes ret1 ret2
        argsSame = all (uncurry exactCompareTypes) (zip args1 args2)

checkProg :: Program -> TCM ()
checkProg (Program _ topDefs) = do
  checkTopDefs topDefs

executeProgramCheck :: Program -> Either TCMException ()
executeProgramCheck program = res
  where
    checkMonad = checkProg program
    (res, state, written) = runTCM checkMonad emptyTCMEnv emptyTCMState
