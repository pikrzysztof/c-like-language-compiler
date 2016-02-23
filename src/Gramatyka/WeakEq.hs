{-# LANGUAGE GADTs #-}
module Gramatyka.WeakEq where

import Gramatyka.AbsLatte

class WeakEq a where
  weakEq :: a -> a -> Bool

instance WeakEq (Tree a) where
   weakEq (Prgm  _) (Prgm  _) = True
   weakEq (FnDef _ _ _ _) (FnDef _ _ _ _) = True
   weakEq (Argument _ _) (Argument _ _) = True
   weakEq (Blk _) (Blk _) = True
   weakEq (Empty ) (Empty ) = True
   weakEq (BStmt _) (BStmt _) = True
   weakEq (Decl _ _) (Decl _ _) = True
   weakEq (Ass _ _) (Ass _ _) = True
   weakEq (Ret _) (Ret _) = True
   weakEq (VRet ) (VRet ) = True
   weakEq (CondElse _ _ _) (CondElse _ _ _) = True
   weakEq (While _ _) (While _ _) = True
   weakEq (SExp _) (SExp _) = True
   weakEq (Init _ _) (Init _ _) = True
   weakEq (Int ) (Int ) = True
   weakEq (Str ) (Str ) = True
   weakEq (Bool ) (Bool ) = True
   weakEq (IdentType _) (IdentType _) = True
   weakEq (Void ) (Void ) = True
   weakEq (TECast _ _) (TECast _ _) = True
   weakEq (TELValue _ _) (TELValue _ _) = True
   weakEq (TEConstr _ _) (TEConstr _ _) = True
   weakEq (TELitInt _ _) (TELitInt _ _) = True
   weakEq (TELitTrue _) (TELitTrue _) = True
   weakEq (TELitFalse _) (TELitFalse _) = True
   weakEq (TEMethApp _ _ _ _) (TEMethApp _ _ _ _) = True
   weakEq (TEApp _ _ _) (TEApp _ _ _) = True
   weakEq (TEString _ _) (TEString _ _) = True
   weakEq (TNeg _ _) (TNeg _ _) = True
   weakEq (TNot _ _) (TNot _ _) = True
   weakEq (TEMul _ _ _ _) (TEMul _ _ _ _) = True
   weakEq (TEAdd _ _ _ _) (TEAdd _ _ _ _) = True
   weakEq (TERel _ _ _ _) (TERel _ _ _ _) = True
   weakEq (TEAnd _ _ _) (TEAnd _ _ _) = True
   weakEq (TEOr _ _ _) (TEOr _ _ _) = True
   weakEq (Plus ) (Plus ) = True
   weakEq (Times ) (Times ) = True
   weakEq (Div ) (Div ) = True
   weakEq (Mod ) (Mod ) = True
   weakEq (GTH ) (GTH ) = True
   weakEq (GE ) (GE ) = True
   weakEq (EQU ) (EQU ) = True
   weakEq (Ident _) (Ident _) = True
   weakEq (Fun _ _) (Fun _ _) = True
   weakEq (LVIdent _) (LVIdent _) = True
   weakEq (ECast _) (ECast _) = True
   weakEq (ELValue _) (ELValue _) = True
   weakEq (EConstr _) (EConstr _) = True
   weakEq (ELitInt _) (ELitInt _) = True
   weakEq (ELitTrue ) (ELitTrue ) = True
   weakEq (ELitFalse ) (ELitFalse ) = True
   weakEq (EMethApp _ _ _) (EMethApp _ _ _) = True
   weakEq (EApp _ _) (EApp _ _) = True
   weakEq (EString _) (EString _) = True
   weakEq (Neg _) (Neg _) = True
   weakEq (Not _) (Not _) = True
   weakEq (EMul _ _ _) (EMul _ _ _) = True
   weakEq (EAdd _ _ _) (EAdd _ _ _) = True
   weakEq (ERel _ _ _) (ERel _ _ _) = True
   weakEq (EAnd _ _) (EAnd _ _) = True
   weakEq (EOr _ _) (EOr _ _) = True
   weakEq (NoInit _) (NoInit _) = True
   weakEq (Incr _) (Incr _) = True
   weakEq (Decr _) (Decr _) = True
   weakEq (Cond _ _) (Cond _ _) = True
   weakEq (NE ) (NE ) = True
   weakEq (LTH ) (LTH ) = True
   weakEq (Minus ) (Minus ) = True
   weakEq (LE ) (LE ) = True
   weakEq (LVMember _ _) (LVMember _ _) = True
   weakEq (LVArrItem _ _) (LVArrItem _ _) = True
   weakEq (ArrType _) (ArrType _) = True
   weakEq (ClsDef _ _) (ClsDef _ _) = True
   weakEq (ClsExtDef _ _ _) (ClsExtDef _ _ _) = True
   weakEq (MemberDef _ _) (MemberDef _ _) = True
   weakEq (MethodDef _ _ _ _) (MethodDef _ _ _ _) = True
   weakEq _ _ = False
