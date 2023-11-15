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

noLoc :: BNFC'Position
noLoc = Nothing

-- Monad elements
type TCMException = String
data TCMEnv = LCMState {
  envTypes :: TypesMap,
  envReturnType :: Type,
  envStructTypes :: StructTypes
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

-- Show location in code
showLoc :: BNFC'Position -> String
showLoc Nothing = "[Position unknown]"
showLoc (Just (line, col)) = "line " ++ sLine ++ ", colum " ++ sCol
  where
    sLine = show line
    sCol = show col

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

checkAttr :: BNFC'Position -> Type -> Ident -> TCM Type
checkAttr loc (Struct _ struct) attr = do
  env <- ask
  let attrType = getStructAttrType env struct attr
  case attrType of
    Nothing -> throwError ("Invalid attribute name, at " ++ showLoc loc)
    Just t -> return t
checkAttr loc _ _ = throwError ("Trying to access attribute of non structure, at " ++ showLoc loc)
