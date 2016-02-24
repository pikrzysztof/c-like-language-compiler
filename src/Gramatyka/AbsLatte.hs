{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
module Gramatyka.AbsLatte (Tree(..), Program, TopDef, Arg, ClsBowel, Block, Stmt, Item, Type, ConstrType, OptSize, LValue, Expr, AddOp, MulOp, RelOp, Ident, johnMajorEq, module Gramatyka.ComposOp) where

import Gramatyka.ComposOp

import Data.Monoid (mappend)


-- Haskell module generated by the BNF converter

data Tag = Program_ | TopDef_ | Arg_ | ClsBowel_ | Block_ | Stmt_ | Item_ | Type_ | ConstrType_ | OptSize_ | LValue_ | Expr_ | AddOp_ | MulOp_ | RelOp_ | Ident_
type Program = Tree Program_
type TopDef = Tree TopDef_
type Arg = Tree Arg_
type ClsBowel = Tree ClsBowel_
type Block = Tree Block_
type Stmt = Tree Stmt_
type Item = Tree Item_
type Type = Tree Type_
type ConstrType = Tree ConstrType_
type OptSize = Tree OptSize_
type LValue = Tree LValue_
type Expr = Tree Expr_
type AddOp = Tree AddOp_
type MulOp = Tree MulOp_
type RelOp = Tree RelOp_
type Ident = Tree Ident_

data Tree :: Tag -> * where
    Prgm :: [TopDef] -> Tree Program_
    FnDef :: Type -> Ident -> [Arg] -> Block -> Tree TopDef_
    ClsDef :: Ident -> [ClsBowel] -> Tree TopDef_
    ClsExtDef :: Ident -> Ident -> [ClsBowel] -> Tree TopDef_
    Argument :: Type -> Ident -> Tree Arg_
    MemberDef :: Type -> [Ident] -> Tree ClsBowel_
    MethodDef :: Type -> Ident -> [Arg] -> Block -> Tree ClsBowel_
    Blk :: [Stmt] -> Tree Block_
    Empty :: Tree Stmt_
    BStmt :: Block -> Tree Stmt_
    Decl :: Type -> [Item] -> Tree Stmt_
    Ass :: LValue -> Expr -> Tree Stmt_
    Incr :: LValue -> Tree Stmt_
    Decr :: LValue -> Tree Stmt_
    Ret :: Expr -> Tree Stmt_
    VRet :: Tree Stmt_
    Cond :: Expr -> Stmt -> Tree Stmt_
    CondElse :: Expr -> Stmt -> Stmt -> Tree Stmt_
    While :: Expr -> Stmt -> Tree Stmt_
    SExp :: Expr -> Tree Stmt_
    NoInit :: Ident -> Tree Item_
    Init :: Ident -> Expr -> Tree Item_
    ArrType :: Type -> Tree Type_
    IdentType :: Ident -> Tree Type_
    Int :: Tree Type_
    Str :: Tree Type_
    Bool :: Tree Type_
    Void :: Tree Type_
    Constr :: Type -> OptSize -> Tree ConstrType_
    ArrConstr :: Type -> Expr -> Tree ConstrType_
    ClsConstr :: Type -> Tree ConstrType_
    NoSiz :: Tree OptSize_
    Siz :: Expr -> Tree OptSize_
    LVIdent :: Ident -> Tree LValue_
    LVMember :: Expr -> Ident -> Tree LValue_
    LVArrItem :: Expr -> Expr -> Tree LValue_
    ECast :: Ident -> Tree Expr_
    ELValue :: LValue -> Tree Expr_
    EConstr :: ConstrType -> Tree Expr_
    ELitInt :: Integer -> Tree Expr_
    ELitTrue :: Tree Expr_
    ELitFalse :: Tree Expr_
    EMethApp :: Expr -> Ident -> [Expr] -> Tree Expr_
    EApp :: Ident -> [Expr] -> Tree Expr_
    EString :: String -> Tree Expr_
    Neg :: Expr -> Tree Expr_
    Not :: Expr -> Tree Expr_
    EMul :: Expr -> MulOp -> Expr -> Tree Expr_
    EAdd :: Expr -> AddOp -> Expr -> Tree Expr_
    ERel :: Expr -> RelOp -> Expr -> Tree Expr_
    EAnd :: Expr -> Expr -> Tree Expr_
    EOr :: Expr -> Expr -> Tree Expr_
    TECast :: Type -> Ident -> Tree Expr_
    TELValue :: Type -> LValue -> Tree Expr_
    TEConstr :: Type -> ConstrType -> Tree Expr_
    TELitInt :: Type -> Integer -> Tree Expr_
    TELitTrue :: Type -> Tree Expr_
    TELitFalse :: Type -> Tree Expr_
    TEMethApp :: Type -> Expr -> Ident -> [Expr] -> Tree Expr_
    TEApp :: Type -> Ident -> [Expr] -> Tree Expr_
    TEString :: Type -> String -> Tree Expr_
    TNeg :: Type -> Expr -> Tree Expr_
    TNot :: Type -> Expr -> Tree Expr_
    TEMul :: Type -> Expr -> MulOp -> Expr -> Tree Expr_
    TEAdd :: Type -> Expr -> AddOp -> Expr -> Tree Expr_
    TERel :: Type -> Expr -> RelOp -> Expr -> Tree Expr_
    TEAnd :: Type -> Expr -> Expr -> Tree Expr_
    TEOr :: Type -> Expr -> Expr -> Tree Expr_
    Plus :: Tree AddOp_
    Minus :: Tree AddOp_
    Times :: Tree MulOp_
    Div :: Tree MulOp_
    Mod :: Tree MulOp_
    LTH :: Tree RelOp_
    LE :: Tree RelOp_
    GTH :: Tree RelOp_
    GE :: Tree RelOp_
    EQU :: Tree RelOp_
    NE :: Tree RelOp_
    Ident :: String -> Tree Ident_

instance Compos Tree where
  compos r a f t = case t of
      Prgm topdefs -> r Prgm `a` foldr (a . a (r (:)) . f) (r []) topdefs
      FnDef type' ident args block -> r FnDef `a` f type' `a` f ident `a` foldr (a . a (r (:)) . f) (r []) args `a` f block
      ClsDef ident clsbowels -> r ClsDef `a` f ident `a` foldr (a . a (r (:)) . f) (r []) clsbowels
      ClsExtDef ident0 ident1 clsbowels2 -> r ClsExtDef `a` f ident0 `a` f ident1 `a` foldr (a . a (r (:)) . f) (r []) clsbowels2
      Argument type' ident -> r Argument `a` f type' `a` f ident
      MemberDef type' idents -> r MemberDef `a` f type' `a` foldr (a . a (r (:)) . f) (r []) idents
      MethodDef type' ident args block -> r MethodDef `a` f type' `a` f ident `a` foldr (a . a (r (:)) . f) (r []) args `a` f block
      Blk stmts -> r Blk `a` foldr (a . a (r (:)) . f) (r []) stmts
      BStmt block -> r BStmt `a` f block
      Decl type' items -> r Decl `a` f type' `a` foldr (a . a (r (:)) . f) (r []) items
      Ass lvalue expr -> r Ass `a` f lvalue `a` f expr
      Incr lvalue -> r Incr `a` f lvalue
      Decr lvalue -> r Decr `a` f lvalue
      Ret expr -> r Ret `a` f expr
      Cond expr stmt -> r Cond `a` f expr `a` f stmt
      CondElse expr stmt0 stmt1 -> r CondElse `a` f expr `a` f stmt0 `a` f stmt1
      While expr stmt -> r While `a` f expr `a` f stmt
      SExp expr -> r SExp `a` f expr
      NoInit ident -> r NoInit `a` f ident
      Init ident expr -> r Init `a` f ident `a` f expr
      ArrType type' -> r ArrType `a` f type'
      IdentType ident -> r IdentType `a` f ident
      Constr type' optsize -> r Constr `a` f type' `a` f optsize
      ArrConstr type' expr -> r ArrConstr `a` f type' `a` f expr
      ClsConstr type' -> r ClsConstr `a` f type'
      Siz expr -> r Siz `a` f expr
      LVIdent ident -> r LVIdent `a` f ident
      LVMember expr ident -> r LVMember `a` f expr `a` f ident
      LVArrItem expr0 expr1 -> r LVArrItem `a` f expr0 `a` f expr1
      ECast ident -> r ECast `a` f ident
      ELValue lvalue -> r ELValue `a` f lvalue
      EConstr constrtype -> r EConstr `a` f constrtype
      EMethApp expr ident exprs -> r EMethApp `a` f expr `a` f ident `a` foldr (a . a (r (:)) . f) (r []) exprs
      EApp ident exprs -> r EApp `a` f ident `a` foldr (a . a (r (:)) . f) (r []) exprs
      Neg expr -> r Neg `a` f expr
      Not expr -> r Not `a` f expr
      EMul expr0 mulop1 expr2 -> r EMul `a` f expr0 `a` f mulop1 `a` f expr2
      EAdd expr0 addop1 expr2 -> r EAdd `a` f expr0 `a` f addop1 `a` f expr2
      ERel expr0 relop1 expr2 -> r ERel `a` f expr0 `a` f relop1 `a` f expr2
      EAnd expr0 expr1 -> r EAnd `a` f expr0 `a` f expr1
      EOr expr0 expr1 -> r EOr `a` f expr0 `a` f expr1
      TECast type' ident -> r TECast `a` f type' `a` f ident
      TELValue type' lvalue -> r TELValue `a` f type' `a` f lvalue
      TEConstr type' constrtype -> r TEConstr `a` f type' `a` f constrtype
      TELitInt type' integer -> r TELitInt `a` f type' `a` r integer
      TELitTrue type' -> r TELitTrue `a` f type'
      TELitFalse type' -> r TELitFalse `a` f type'
      TEMethApp type' expr ident exprs -> r TEMethApp `a` f type' `a` f expr `a` f ident `a` foldr (a . a (r (:)) . f) (r []) exprs
      TEApp type' ident exprs -> r TEApp `a` f type' `a` f ident `a` foldr (a . a (r (:)) . f) (r []) exprs
      TEString type' string -> r TEString `a` f type' `a` r string
      TNeg type' expr -> r TNeg `a` f type' `a` f expr
      TNot type' expr -> r TNot `a` f type' `a` f expr
      TEMul type' expr0 mulop1 expr2 -> r TEMul `a` f type' `a` f expr0 `a` f mulop1 `a` f expr2
      TEAdd type' expr0 addop1 expr2 -> r TEAdd `a` f type' `a` f expr0 `a` f addop1 `a` f expr2
      TERel type' expr0 relop1 expr2 -> r TERel `a` f type' `a` f expr0 `a` f relop1 `a` f expr2
      TEAnd type' expr0 expr1 -> r TEAnd `a` f type' `a` f expr0 `a` f expr1
      TEOr type' expr0 expr1 -> r TEOr `a` f type' `a` f expr0 `a` f expr1
      _ -> r t

instance Show (Tree c) where
  showsPrec n t = case t of
    Prgm topdefs -> opar n . showString "Prgm" . showChar ' ' . showsPrec 1 topdefs . cpar n
    FnDef type' ident args block -> opar n . showString "FnDef" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 args . showChar ' ' . showsPrec 1 block . cpar n
    ClsDef ident clsbowels -> opar n . showString "ClsDef" . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 clsbowels . cpar n
    ClsExtDef ident0 ident1 clsbowels2 -> opar n . showString "ClsExtDef" . showChar ' ' . showsPrec 1 ident0 . showChar ' ' . showsPrec 1 ident1 . showChar ' ' . showsPrec 1 clsbowels2 . cpar n
    Argument type' ident -> opar n . showString "Argument" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 ident . cpar n
    MemberDef type' idents -> opar n . showString "MemberDef" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 idents . cpar n
    MethodDef type' ident args block -> opar n . showString "MethodDef" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 args . showChar ' ' . showsPrec 1 block . cpar n
    Blk stmts -> opar n . showString "Blk" . showChar ' ' . showsPrec 1 stmts . cpar n
    Empty -> showString "Empty"
    BStmt block -> opar n . showString "BStmt" . showChar ' ' . showsPrec 1 block . cpar n
    Decl type' items -> opar n . showString "Decl" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 items . cpar n
    Ass lvalue expr -> opar n . showString "Ass" . showChar ' ' . showsPrec 1 lvalue . showChar ' ' . showsPrec 1 expr . cpar n
    Incr lvalue -> opar n . showString "Incr" . showChar ' ' . showsPrec 1 lvalue . cpar n
    Decr lvalue -> opar n . showString "Decr" . showChar ' ' . showsPrec 1 lvalue . cpar n
    Ret expr -> opar n . showString "Ret" . showChar ' ' . showsPrec 1 expr . cpar n
    VRet -> showString "VRet"
    Cond expr stmt -> opar n . showString "Cond" . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 stmt . cpar n
    CondElse expr stmt0 stmt1 -> opar n . showString "CondElse" . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 stmt0 . showChar ' ' . showsPrec 1 stmt1 . cpar n
    While expr stmt -> opar n . showString "While" . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 stmt . cpar n
    SExp expr -> opar n . showString "SExp" . showChar ' ' . showsPrec 1 expr . cpar n
    NoInit ident -> opar n . showString "NoInit" . showChar ' ' . showsPrec 1 ident . cpar n
    Init ident expr -> opar n . showString "Init" . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 expr . cpar n
    ArrType type' -> opar n . showString "ArrType" . showChar ' ' . showsPrec 1 type' . cpar n
    IdentType ident -> opar n . showString "IdentType" . showChar ' ' . showsPrec 1 ident . cpar n
    Int -> showString "Int"
    Str -> showString "Str"
    Bool -> showString "Bool"
    Void -> showString "Void"
    Constr type' optsize -> opar n . showString "Constr" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 optsize . cpar n
    ArrConstr type' expr -> opar n . showString "ArrConstr" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr . cpar n
    ClsConstr type' -> opar n . showString "ClsConstr" . showChar ' ' . showsPrec 1 type' . cpar n
    NoSiz -> showString "NoSiz"
    Siz expr -> opar n . showString "Siz" . showChar ' ' . showsPrec 1 expr . cpar n
    LVIdent ident -> opar n . showString "LVIdent" . showChar ' ' . showsPrec 1 ident . cpar n
    LVMember expr ident -> opar n . showString "LVMember" . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 ident . cpar n
    LVArrItem expr0 expr1 -> opar n . showString "LVArrItem" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 expr1 . cpar n
    ECast ident -> opar n . showString "ECast" . showChar ' ' . showsPrec 1 ident . cpar n
    ELValue lvalue -> opar n . showString "ELValue" . showChar ' ' . showsPrec 1 lvalue . cpar n
    EConstr constrtype -> opar n . showString "EConstr" . showChar ' ' . showsPrec 1 constrtype . cpar n
    ELitInt integer -> opar n . showString "ELitInt" . showChar ' ' . showsPrec 1 integer . cpar n
    ELitTrue -> showString "ELitTrue"
    ELitFalse -> showString "ELitFalse"
    EMethApp expr ident exprs -> opar n . showString "EMethApp" . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 exprs . cpar n
    EApp ident exprs -> opar n . showString "EApp" . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 exprs . cpar n
    EString string -> opar n . showString "EString" . showChar ' ' . showsPrec 1 string . cpar n
    Neg expr -> opar n . showString "Neg" . showChar ' ' . showsPrec 1 expr . cpar n
    Not expr -> opar n . showString "Not" . showChar ' ' . showsPrec 1 expr . cpar n
    EMul expr0 mulop1 expr2 -> opar n . showString "EMul" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 mulop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    EAdd expr0 addop1 expr2 -> opar n . showString "EAdd" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 addop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    ERel expr0 relop1 expr2 -> opar n . showString "ERel" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 relop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    EAnd expr0 expr1 -> opar n . showString "EAnd" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 expr1 . cpar n
    EOr expr0 expr1 -> opar n . showString "EOr" . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 expr1 . cpar n
    TECast type' ident -> opar n . showString "TECast" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 ident . cpar n
    TELValue type' lvalue -> opar n . showString "TELValue" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 lvalue . cpar n
    TEConstr type' constrtype -> opar n . showString "TEConstr" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 constrtype . cpar n
    TELitInt type' integer -> opar n . showString "TELitInt" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 integer . cpar n
    TELitTrue type' -> opar n . showString "TELitTrue" . showChar ' ' . showsPrec 1 type' . cpar n
    TELitFalse type' -> opar n . showString "TELitFalse" . showChar ' ' . showsPrec 1 type' . cpar n
    TEMethApp type' expr ident exprs -> opar n . showString "TEMethApp" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 exprs . cpar n
    TEApp type' ident exprs -> opar n . showString "TEApp" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 ident . showChar ' ' . showsPrec 1 exprs . cpar n
    TEString type' string -> opar n . showString "TEString" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 string . cpar n
    TNeg type' expr -> opar n . showString "TNeg" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr . cpar n
    TNot type' expr -> opar n . showString "TNot" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr . cpar n
    TEMul type' expr0 mulop1 expr2 -> opar n . showString "TEMul" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 mulop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    TEAdd type' expr0 addop1 expr2 -> opar n . showString "TEAdd" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 addop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    TERel type' expr0 relop1 expr2 -> opar n . showString "TERel" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 relop1 . showChar ' ' . showsPrec 1 expr2 . cpar n
    TEAnd type' expr0 expr1 -> opar n . showString "TEAnd" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 expr1 . cpar n
    TEOr type' expr0 expr1 -> opar n . showString "TEOr" . showChar ' ' . showsPrec 1 type' . showChar ' ' . showsPrec 1 expr0 . showChar ' ' . showsPrec 1 expr1 . cpar n
    Plus -> showString "Plus"
    Minus -> showString "Minus"
    Times -> showString "Times"
    Div -> showString "Div"
    Mod -> showString "Mod"
    LTH -> showString "LTH"
    LE -> showString "LE"
    GTH -> showString "GTH"
    GE -> showString "GE"
    EQU -> showString "EQU"
    NE -> showString "NE"
    Ident str -> opar n . showString "Ident" . showChar ' ' . showsPrec 1 str . cpar n
   where opar n = if n > 0 then showChar '(' else id
         cpar n = if n > 0 then showChar ')' else id

instance Eq (Tree c) where (==) = johnMajorEq

johnMajorEq :: Tree a -> Tree b -> Bool
johnMajorEq (Prgm topdefs) (Prgm topdefs_) = topdefs == topdefs_
johnMajorEq (FnDef type' ident args block) (FnDef type'_ ident_ args_ block_) = type' == type'_ && ident == ident_ && args == args_ && block == block_
johnMajorEq (ClsDef ident clsbowels) (ClsDef ident_ clsbowels_) = ident == ident_ && clsbowels == clsbowels_
johnMajorEq (ClsExtDef ident0 ident1 clsbowels2) (ClsExtDef ident0_ ident1_ clsbowels2_) = ident0 == ident0_ && ident1 == ident1_ && clsbowels2 == clsbowels2_
johnMajorEq (Argument type' ident) (Argument type'_ ident_) = type' == type'_ && ident == ident_
johnMajorEq (MemberDef type' idents) (MemberDef type'_ idents_) = type' == type'_ && idents == idents_
johnMajorEq (MethodDef type' ident args block) (MethodDef type'_ ident_ args_ block_) = type' == type'_ && ident == ident_ && args == args_ && block == block_
johnMajorEq (Blk stmts) (Blk stmts_) = stmts == stmts_
johnMajorEq Empty Empty = True
johnMajorEq (BStmt block) (BStmt block_) = block == block_
johnMajorEq (Decl type' items) (Decl type'_ items_) = type' == type'_ && items == items_
johnMajorEq (Ass lvalue expr) (Ass lvalue_ expr_) = lvalue == lvalue_ && expr == expr_
johnMajorEq (Incr lvalue) (Incr lvalue_) = lvalue == lvalue_
johnMajorEq (Decr lvalue) (Decr lvalue_) = lvalue == lvalue_
johnMajorEq (Ret expr) (Ret expr_) = expr == expr_
johnMajorEq VRet VRet = True
johnMajorEq (Cond expr stmt) (Cond expr_ stmt_) = expr == expr_ && stmt == stmt_
johnMajorEq (CondElse expr stmt0 stmt1) (CondElse expr_ stmt0_ stmt1_) = expr == expr_ && stmt0 == stmt0_ && stmt1 == stmt1_
johnMajorEq (While expr stmt) (While expr_ stmt_) = expr == expr_ && stmt == stmt_
johnMajorEq (SExp expr) (SExp expr_) = expr == expr_
johnMajorEq (NoInit ident) (NoInit ident_) = ident == ident_
johnMajorEq (Init ident expr) (Init ident_ expr_) = ident == ident_ && expr == expr_
johnMajorEq (ArrType type') (ArrType type'_) = type' == type'_
johnMajorEq (IdentType ident) (IdentType ident_) = ident == ident_
johnMajorEq Int Int = True
johnMajorEq Str Str = True
johnMajorEq Bool Bool = True
johnMajorEq Void Void = True
johnMajorEq (Constr type' optsize) (Constr type'_ optsize_) = type' == type'_ && optsize == optsize_
johnMajorEq (ArrConstr type' expr) (ArrConstr type'_ expr_) = type' == type'_ && expr == expr_
johnMajorEq (ClsConstr type') (ClsConstr type'_) = type' == type'_
johnMajorEq NoSiz NoSiz = True
johnMajorEq (Siz expr) (Siz expr_) = expr == expr_
johnMajorEq (LVIdent ident) (LVIdent ident_) = ident == ident_
johnMajorEq (LVMember expr ident) (LVMember expr_ ident_) = expr == expr_ && ident == ident_
johnMajorEq (LVArrItem expr0 expr1) (LVArrItem expr0_ expr1_) = expr0 == expr0_ && expr1 == expr1_
johnMajorEq (ECast ident) (ECast ident_) = ident == ident_
johnMajorEq (ELValue lvalue) (ELValue lvalue_) = lvalue == lvalue_
johnMajorEq (EConstr constrtype) (EConstr constrtype_) = constrtype == constrtype_
johnMajorEq (ELitInt integer) (ELitInt integer_) = integer == integer_
johnMajorEq ELitTrue ELitTrue = True
johnMajorEq ELitFalse ELitFalse = True
johnMajorEq (EMethApp expr ident exprs) (EMethApp expr_ ident_ exprs_) = expr == expr_ && ident == ident_ && exprs == exprs_
johnMajorEq (EApp ident exprs) (EApp ident_ exprs_) = ident == ident_ && exprs == exprs_
johnMajorEq (EString string) (EString string_) = string == string_
johnMajorEq (Neg expr) (Neg expr_) = expr == expr_
johnMajorEq (Not expr) (Not expr_) = expr == expr_
johnMajorEq (EMul expr0 mulop1 expr2) (EMul expr0_ mulop1_ expr2_) = expr0 == expr0_ && mulop1 == mulop1_ && expr2 == expr2_
johnMajorEq (EAdd expr0 addop1 expr2) (EAdd expr0_ addop1_ expr2_) = expr0 == expr0_ && addop1 == addop1_ && expr2 == expr2_
johnMajorEq (ERel expr0 relop1 expr2) (ERel expr0_ relop1_ expr2_) = expr0 == expr0_ && relop1 == relop1_ && expr2 == expr2_
johnMajorEq (EAnd expr0 expr1) (EAnd expr0_ expr1_) = expr0 == expr0_ && expr1 == expr1_
johnMajorEq (EOr expr0 expr1) (EOr expr0_ expr1_) = expr0 == expr0_ && expr1 == expr1_
johnMajorEq (TECast type' ident) (TECast type'_ ident_) = type' == type'_ && ident == ident_
johnMajorEq (TELValue type' lvalue) (TELValue type'_ lvalue_) = type' == type'_ && lvalue == lvalue_
johnMajorEq (TEConstr type' constrtype) (TEConstr type'_ constrtype_) = type' == type'_ && constrtype == constrtype_
johnMajorEq (TELitInt type' integer) (TELitInt type'_ integer_) = type' == type'_ && integer == integer_
johnMajorEq (TELitTrue type') (TELitTrue type'_) = type' == type'_
johnMajorEq (TELitFalse type') (TELitFalse type'_) = type' == type'_
johnMajorEq (TEMethApp type' expr ident exprs) (TEMethApp type'_ expr_ ident_ exprs_) = type' == type'_ && expr == expr_ && ident == ident_ && exprs == exprs_
johnMajorEq (TEApp type' ident exprs) (TEApp type'_ ident_ exprs_) = type' == type'_ && ident == ident_ && exprs == exprs_
johnMajorEq (TEString type' string) (TEString type'_ string_) = type' == type'_ && string == string_
johnMajorEq (TNeg type' expr) (TNeg type'_ expr_) = type' == type'_ && expr == expr_
johnMajorEq (TNot type' expr) (TNot type'_ expr_) = type' == type'_ && expr == expr_
johnMajorEq (TEMul type' expr0 mulop1 expr2) (TEMul type'_ expr0_ mulop1_ expr2_) = type' == type'_ && expr0 == expr0_ && mulop1 == mulop1_ && expr2 == expr2_
johnMajorEq (TEAdd type' expr0 addop1 expr2) (TEAdd type'_ expr0_ addop1_ expr2_) = type' == type'_ && expr0 == expr0_ && addop1 == addop1_ && expr2 == expr2_
johnMajorEq (TERel type' expr0 relop1 expr2) (TERel type'_ expr0_ relop1_ expr2_) = type' == type'_ && expr0 == expr0_ && relop1 == relop1_ && expr2 == expr2_
johnMajorEq (TEAnd type' expr0 expr1) (TEAnd type'_ expr0_ expr1_) = type' == type'_ && expr0 == expr0_ && expr1 == expr1_
johnMajorEq (TEOr type' expr0 expr1) (TEOr type'_ expr0_ expr1_) = type' == type'_ && expr0 == expr0_ && expr1 == expr1_
johnMajorEq Plus Plus = True
johnMajorEq Minus Minus = True
johnMajorEq Times Times = True
johnMajorEq Div Div = True
johnMajorEq Mod Mod = True
johnMajorEq LTH LTH = True
johnMajorEq LE LE = True
johnMajorEq GTH GTH = True
johnMajorEq GE GE = True
johnMajorEq EQU EQU = True
johnMajorEq NE NE = True
johnMajorEq (Ident str) (Ident str_) = str == str_
johnMajorEq _ _ = False

instance Ord (Tree c) where
  compare x y = compare (index x) (index y) `mappend` compareSame x y
index :: Tree c -> Int
index (Prgm _) = 0
index (FnDef _ _ _ _) = 1
index (ClsDef _ _) = 2
index (ClsExtDef _ _ _) = 3
index (Argument _ _) = 4
index (MemberDef _ _) = 5
index (MethodDef _ _ _ _) = 6
index (Blk _) = 7
index (Empty ) = 8
index (BStmt _) = 9
index (Decl _ _) = 10
index (Ass _ _) = 11
index (Incr _) = 12
index (Decr _) = 13
index (Ret _) = 14
index (VRet ) = 15
index (Cond _ _) = 16
index (CondElse _ _ _) = 17
index (While _ _) = 18
index (SExp _) = 19
index (NoInit _) = 20
index (Init _ _) = 21
index (ArrType _) = 22
index (IdentType _) = 23
index (Int ) = 24
index (Str ) = 25
index (Bool ) = 26
index (Void ) = 27
index (Constr _ _) = 28
index (ArrConstr _ _) = 29
index (ClsConstr _) = 30
index (NoSiz ) = 31
index (Siz _) = 32
index (LVIdent _) = 33
index (LVMember _ _) = 34
index (LVArrItem _ _) = 35
index (ECast _) = 36
index (ELValue _) = 37
index (EConstr _) = 38
index (ELitInt _) = 39
index (ELitTrue ) = 40
index (ELitFalse ) = 41
index (EMethApp _ _ _) = 42
index (EApp _ _) = 43
index (EString _) = 44
index (Neg _) = 45
index (Not _) = 46
index (EMul _ _ _) = 47
index (EAdd _ _ _) = 48
index (ERel _ _ _) = 49
index (EAnd _ _) = 50
index (EOr _ _) = 51
index (TECast _ _) = 52
index (TELValue _ _) = 53
index (TEConstr _ _) = 54
index (TELitInt _ _) = 55
index (TELitTrue _) = 56
index (TELitFalse _) = 57
index (TEMethApp _ _ _ _) = 58
index (TEApp _ _ _) = 59
index (TEString _ _) = 60
index (TNeg _ _) = 61
index (TNot _ _) = 62
index (TEMul _ _ _ _) = 63
index (TEAdd _ _ _ _) = 64
index (TERel _ _ _ _) = 65
index (TEAnd _ _ _) = 66
index (TEOr _ _ _) = 67
index (Plus ) = 68
index (Minus ) = 69
index (Times ) = 70
index (Div ) = 71
index (Mod ) = 72
index (LTH ) = 73
index (LE ) = 74
index (GTH ) = 75
index (GE ) = 76
index (EQU ) = 77
index (NE ) = 78
index (Ident _) = 79
compareSame :: Tree c -> Tree c -> Ordering
compareSame (Prgm topdefs) (Prgm topdefs_) = compare topdefs topdefs_
compareSame (FnDef type' ident args block) (FnDef type'_ ident_ args_ block_) = mappend (compare type' type'_) (mappend (compare ident ident_) (mappend (compare args args_) (compare block block_)))
compareSame (ClsDef ident clsbowels) (ClsDef ident_ clsbowels_) = mappend (compare ident ident_) (compare clsbowels clsbowels_)
compareSame (ClsExtDef ident0 ident1 clsbowels2) (ClsExtDef ident0_ ident1_ clsbowels2_) = mappend (compare ident0 ident0_) (mappend (compare ident1 ident1_) (compare clsbowels2 clsbowels2_))
compareSame (Argument type' ident) (Argument type'_ ident_) = mappend (compare type' type'_) (compare ident ident_)
compareSame (MemberDef type' idents) (MemberDef type'_ idents_) = mappend (compare type' type'_) (compare idents idents_)
compareSame (MethodDef type' ident args block) (MethodDef type'_ ident_ args_ block_) = mappend (compare type' type'_) (mappend (compare ident ident_) (mappend (compare args args_) (compare block block_)))
compareSame (Blk stmts) (Blk stmts_) = compare stmts stmts_
compareSame Empty Empty = EQ
compareSame (BStmt block) (BStmt block_) = compare block block_
compareSame (Decl type' items) (Decl type'_ items_) = mappend (compare type' type'_) (compare items items_)
compareSame (Ass lvalue expr) (Ass lvalue_ expr_) = mappend (compare lvalue lvalue_) (compare expr expr_)
compareSame (Incr lvalue) (Incr lvalue_) = compare lvalue lvalue_
compareSame (Decr lvalue) (Decr lvalue_) = compare lvalue lvalue_
compareSame (Ret expr) (Ret expr_) = compare expr expr_
compareSame VRet VRet = EQ
compareSame (Cond expr stmt) (Cond expr_ stmt_) = mappend (compare expr expr_) (compare stmt stmt_)
compareSame (CondElse expr stmt0 stmt1) (CondElse expr_ stmt0_ stmt1_) = mappend (compare expr expr_) (mappend (compare stmt0 stmt0_) (compare stmt1 stmt1_))
compareSame (While expr stmt) (While expr_ stmt_) = mappend (compare expr expr_) (compare stmt stmt_)
compareSame (SExp expr) (SExp expr_) = compare expr expr_
compareSame (NoInit ident) (NoInit ident_) = compare ident ident_
compareSame (Init ident expr) (Init ident_ expr_) = mappend (compare ident ident_) (compare expr expr_)
compareSame (ArrType type') (ArrType type'_) = compare type' type'_
compareSame (IdentType ident) (IdentType ident_) = compare ident ident_
compareSame Int Int = EQ
compareSame Str Str = EQ
compareSame Bool Bool = EQ
compareSame Void Void = EQ
compareSame (Constr type' optsize) (Constr type'_ optsize_) = mappend (compare type' type'_) (compare optsize optsize_)
compareSame (ArrConstr type' expr) (ArrConstr type'_ expr_) = mappend (compare type' type'_) (compare expr expr_)
compareSame (ClsConstr type') (ClsConstr type'_) = compare type' type'_
compareSame NoSiz NoSiz = EQ
compareSame (Siz expr) (Siz expr_) = compare expr expr_
compareSame (LVIdent ident) (LVIdent ident_) = compare ident ident_
compareSame (LVMember expr ident) (LVMember expr_ ident_) = mappend (compare expr expr_) (compare ident ident_)
compareSame (LVArrItem expr0 expr1) (LVArrItem expr0_ expr1_) = mappend (compare expr0 expr0_) (compare expr1 expr1_)
compareSame (ECast ident) (ECast ident_) = compare ident ident_
compareSame (ELValue lvalue) (ELValue lvalue_) = compare lvalue lvalue_
compareSame (EConstr constrtype) (EConstr constrtype_) = compare constrtype constrtype_
compareSame (ELitInt integer) (ELitInt integer_) = compare integer integer_
compareSame ELitTrue ELitTrue = EQ
compareSame ELitFalse ELitFalse = EQ
compareSame (EMethApp expr ident exprs) (EMethApp expr_ ident_ exprs_) = mappend (compare expr expr_) (mappend (compare ident ident_) (compare exprs exprs_))
compareSame (EApp ident exprs) (EApp ident_ exprs_) = mappend (compare ident ident_) (compare exprs exprs_)
compareSame (EString string) (EString string_) = compare string string_
compareSame (Neg expr) (Neg expr_) = compare expr expr_
compareSame (Not expr) (Not expr_) = compare expr expr_
compareSame (EMul expr0 mulop1 expr2) (EMul expr0_ mulop1_ expr2_) = mappend (compare expr0 expr0_) (mappend (compare mulop1 mulop1_) (compare expr2 expr2_))
compareSame (EAdd expr0 addop1 expr2) (EAdd expr0_ addop1_ expr2_) = mappend (compare expr0 expr0_) (mappend (compare addop1 addop1_) (compare expr2 expr2_))
compareSame (ERel expr0 relop1 expr2) (ERel expr0_ relop1_ expr2_) = mappend (compare expr0 expr0_) (mappend (compare relop1 relop1_) (compare expr2 expr2_))
compareSame (EAnd expr0 expr1) (EAnd expr0_ expr1_) = mappend (compare expr0 expr0_) (compare expr1 expr1_)
compareSame (EOr expr0 expr1) (EOr expr0_ expr1_) = mappend (compare expr0 expr0_) (compare expr1 expr1_)
compareSame (TECast type' ident) (TECast type'_ ident_) = mappend (compare type' type'_) (compare ident ident_)
compareSame (TELValue type' lvalue) (TELValue type'_ lvalue_) = mappend (compare type' type'_) (compare lvalue lvalue_)
compareSame (TEConstr type' constrtype) (TEConstr type'_ constrtype_) = mappend (compare type' type'_) (compare constrtype constrtype_)
compareSame (TELitInt type' integer) (TELitInt type'_ integer_) = mappend (compare type' type'_) (compare integer integer_)
compareSame (TELitTrue type') (TELitTrue type'_) = compare type' type'_
compareSame (TELitFalse type') (TELitFalse type'_) = compare type' type'_
compareSame (TEMethApp type' expr ident exprs) (TEMethApp type'_ expr_ ident_ exprs_) = mappend (compare type' type'_) (mappend (compare expr expr_) (mappend (compare ident ident_) (compare exprs exprs_)))
compareSame (TEApp type' ident exprs) (TEApp type'_ ident_ exprs_) = mappend (compare type' type'_) (mappend (compare ident ident_) (compare exprs exprs_))
compareSame (TEString type' string) (TEString type'_ string_) = mappend (compare type' type'_) (compare string string_)
compareSame (TNeg type' expr) (TNeg type'_ expr_) = mappend (compare type' type'_) (compare expr expr_)
compareSame (TNot type' expr) (TNot type'_ expr_) = mappend (compare type' type'_) (compare expr expr_)
compareSame (TEMul type' expr0 mulop1 expr2) (TEMul type'_ expr0_ mulop1_ expr2_) = mappend (compare type' type'_) (mappend (compare expr0 expr0_) (mappend (compare mulop1 mulop1_) (compare expr2 expr2_)))
compareSame (TEAdd type' expr0 addop1 expr2) (TEAdd type'_ expr0_ addop1_ expr2_) = mappend (compare type' type'_) (mappend (compare expr0 expr0_) (mappend (compare addop1 addop1_) (compare expr2 expr2_)))
compareSame (TERel type' expr0 relop1 expr2) (TERel type'_ expr0_ relop1_ expr2_) = mappend (compare type' type'_) (mappend (compare expr0 expr0_) (mappend (compare relop1 relop1_) (compare expr2 expr2_)))
compareSame (TEAnd type' expr0 expr1) (TEAnd type'_ expr0_ expr1_) = mappend (compare type' type'_) (mappend (compare expr0 expr0_) (compare expr1 expr1_))
compareSame (TEOr type' expr0 expr1) (TEOr type'_ expr0_ expr1_) = mappend (compare type' type'_) (mappend (compare expr0 expr0_) (compare expr1 expr1_))
compareSame Plus Plus = EQ
compareSame Minus Minus = EQ
compareSame Times Times = EQ
compareSame Div Div = EQ
compareSame Mod Mod = EQ
compareSame LTH LTH = EQ
compareSame LE LE = EQ
compareSame GTH GTH = EQ
compareSame GE GE = EQ
compareSame EQU EQU = EQ
compareSame NE NE = EQ
compareSame (Ident str) (Ident str_) = compare str str_
compareSame x y = error "BNFC error:" compareSame
