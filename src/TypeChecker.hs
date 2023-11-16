{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- module LlvmCompiler ( LCMException, genLlvmCode ) where

import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map

import Grammar.AbsLatte

type TypesMap = Data.Map.Map Ident Type
type StructType = Data.Map.Map Ident Type
type StructTypes = Data.Map.Map Ident StructType
data FunctionSignature = FunctionSignature Type [Type]
type DeclaredFunctions = Data.Map.Map Ident FunctionSignature

noLoc :: BNFC'Position
noLoc = Nothing

-- Monad elements
type TCMException = String
data TCMEnv = LCMState {
  envTypes :: TypesMap,
  envReturnType :: Type,
  envStructTypes :: StructTypes,
  envFunctions :: DeclaredFunctions
}

type TCM a = ExceptT TCMException (ReaderT TCMEnv Identity) a

runTCM :: TCM a -> TCMEnv -> Either TCMException a
runTCM monad env = res
  where
    res = runIdentity (runReaderT (runExceptT monad) env)

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

checkExpr (EAdd loc expr1 _addOp expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T) of
    (Int _, Int _) -> return (Bool noLoc)
    _ -> throwError ("Arithmetic operation of non integer values, at " ++ showLoc loc)

checkExpr (EMul loc expr1 _addOp expr2) = do
  expr1T <- checkExpr expr1
  expr2T <- checkExpr expr2
  case (expr1T, expr2T) of
    (Int _, Int _) -> return (Bool noLoc)
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

checkStmt :: Stmt -> TCM (TCMEnv -> TCMEnv)

checkStmt (Empty _loc) = return id

checkStmt (Ass loc dest val) = do
  destT <- checkLValue loc dest
  valT <- checkExpr val
  if compareTypes destT valT
    then return id
    else throwError ("Wrong type of assigned value, at " ++ showLoc loc)

checkStmt (Incr loc varId) = do
  t <- checkExpr (EVar loc varId)
  case t of
    Int _ -> return id
    _ -> throwError ("Trying to increment variable of non integer type, at " ++ showLoc loc)

checkStmt (Decr loc varId) = do
  t <- checkExpr (EVar loc varId)
  case t of
    Int _ -> return id
    _ -> throwError ("Trying to decrement variable of non integer type, at " ++ showLoc loc)

checkStmt (SExp loc exp) = do
  checkExpr exp
  return id

-- Check if expression is an L-value and returns it's type
checkLValue :: BNFC'Position -> Expr -> TCM Type

checkLValue _ expr@EVar {} = checkExpr expr

checkLValue _ expr@EAttr {} = checkExpr expr

checkLValue _ expr@EArrEl {} = checkExpr expr

checkLValue loc _ = throwError ("Value at left side of assignment isn't L-value, at " ++ showLoc loc)

