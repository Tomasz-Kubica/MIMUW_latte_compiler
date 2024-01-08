module SimplifyStmt (simplifyStmt) where

import AbsLatte
import SimplifyExp

noLoc :: BNFC'Position
noLoc = Nothing

simplifyStmt :: Stmt -> Stmt

simplifyStmt (Cond _ cond body) = result
  where
    simplifiedCond = simplifyExp cond
    result = case simplifiedCond of
      ELitTrue _ -> simplifyStmt body -- Body always executed
      ELitFalse _ -> Empty noLoc -- Body unreachable
      _ -> Cond noLoc simplifiedCond (simplifyStmt body)

simplifyStmt (CondElse _ cond bodyT bodyF) = result
  where
    simplifiedCond = simplifyExp cond
    result = case simplifiedCond of
      ELitTrue _ -> simplifyStmt bodyT -- BodyT always executed
      ELitFalse _ -> simplifyStmt bodyF -- BodyF always executed
      _ -> CondElse noLoc simplifiedCond (simplifyStmt bodyT) (simplifyStmt bodyF)

simplifyStmt (While _ cond body) = result
  where
    simplifiedCond = simplifyExp cond
    result = case simplifiedCond of
      ELitFalse _ -> Empty noLoc -- Body unreachable
      _ -> While noLoc simplifiedCond (simplifyStmt body)

simplifyStmt other = other
