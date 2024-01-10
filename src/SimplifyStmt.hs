module SimplifyStmt (simplifyStmt, removeDeadStmts) where

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

-- Call this after simplifying all stmts
removeDeadStmts :: [Stmt] -> [Stmt]
removeDeadStmts [] = []
removeDeadStmts (stmt:tail) = if isReturn stmt
  then [stmt']
  else stmt':removeDeadStmts tail
    where
      stmt' = removeDeadInsideStmt stmt

removeDeadInsideStmt :: Stmt -> Stmt
removeDeadInsideStmt (BStmt _ (Block _ stmts)) = BStmt noLoc (Block noLoc (removeDeadStmts stmts))
removeDeadInsideStmt (Cond _ cond body) = Cond noLoc cond (removeDeadInsideStmt body)
removeDeadInsideStmt (CondElse _ cond bodyT bodyF) = CondElse noLoc cond (removeDeadInsideStmt bodyT) (removeDeadInsideStmt bodyF)
removeDeadInsideStmt (While _ cond body) = While noLoc cond (removeDeadInsideStmt body)
removeDeadInsideStmt other = other

isReturn :: Stmt -> Bool
isReturn (Ret _ _) = True
isReturn (VRet _) = True
isReturn (CondElse _ cond bodyT bodyF) = isReturn bodyT && isReturn bodyF
isReturn _ = False
