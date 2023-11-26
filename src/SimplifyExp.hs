
module SimplifyExp (simplifyExp) where

import AbsLatte

noLoc :: BNFC'Position
noLoc = Nothing

simplifyExp :: Expr -> Expr

simplifyExp exp@(EAdd _ e1 op e2) = simplified
  where
    se1 = simplifyExp e1
    se2 = simplifyExp e2
    simplified = case (se1, op, se2) of
      (ELitInt _ v1, Plus _, ELitInt _ v2) -> ELitInt noLoc (v1 + v2)
      (ELitInt _ v1, Minus _, ELitInt _ v2) -> ELitInt noLoc (v1 - v2)
      _ -> exp

simplifyExp exp@(EMul _ e1 op e2) = simplified
  where
    se1 = simplifyExp e1
    se2 = simplifyExp e2
    simplified = case (se1, op, se2) of
      (ELitInt _ v1, Times _, ELitInt _ v2) -> ELitInt noLoc (v1 * v2)
      (ELitInt _ v1, Div _, ELitInt _ v2) -> ELitInt noLoc (v1 `div` v2)
      (ELitInt _ v1, Mod _, ELitInt _ v2) -> ELitInt noLoc (v1 `mod` v2)
      _ -> exp

simplifyExp exp@(Neg _ e) = simplified
  where
    se = simplifyExp e
    simplified = case se of
      ELitInt _ v -> ELitInt noLoc (-v)
      _ -> exp

simplifyExp exp@(ERel _ e1 op e2) = simplified
  where
    se1 = simplifyExp e1
    se2 = simplifyExp e2
    simplified = case (se1, op, se2) of
      (ELitInt _ v1, LTH _, ELitInt _ v2) -> boolToLit (v1 < v2)
      (ELitInt _ v1, LE _, ELitInt _ v2) -> boolToLit (v1 <= v2)
      (ELitInt _ v1, GTH _, ELitInt _ v2) -> boolToLit (v1 > v2)
      (ELitInt _ v1, GE _, ELitInt _ v2) -> boolToLit (v1 >= v2)
      (ELitInt _ v1, EQU _, ELitInt _ v2) -> boolToLit (v1 == v2)
      (ELitInt _ v1, NE _, ELitInt _ v2) -> boolToLit (v1 /= v2)
      (EVar _ id1, EQU _, EVar _ id2) -> boolToLit (id1 == id2) -- comparing variable with itself
      _ -> exp

simplifyExp exp@(Not _ e) = simplified
  where
    se = simplifyExp e
    simplified = case se of
      ELitTrue _ -> ELitFalse noLoc
      ELitFalse _ -> ELitTrue noLoc
      _ -> exp

simplifyExp exp@(EAnd _ e1 e2) = simplified
  where
    se1 = simplifyExp e1
    se2 = simplifyExp e2
    simplified = case (se1, se2) of
      (ELitTrue _, ELitTrue _) -> ELitTrue noLoc
      (ELitFalse _, _) -> ELitFalse noLoc
      (_, ELitFalse _) -> ELitFalse noLoc
      _ -> exp

simplifyExp exp@(EOr _ e1 e2) = simplified
  where
    se1 = simplifyExp e1
    se2 = simplifyExp e2
    simplified = case (se1, se2) of
      (ELitFalse _, ELitFalse _) -> ELitFalse noLoc
      (ELitTrue _, _) -> ELitTrue noLoc
      (_, ELitTrue _) -> ELitTrue noLoc
      _ -> exp

simplifyExp other = other

boolToLit :: Bool -> Expr
boolToLit True = ELitTrue noLoc
boolToLit False = ELitFalse noLoc
