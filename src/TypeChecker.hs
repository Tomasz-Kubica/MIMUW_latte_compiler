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


