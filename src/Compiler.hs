{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs #-}
module Compiler where

import Data.Sequence
import Data.Map as M
import Data.Maybe
import Data.List as L
import CompilerEnv
import CompilerState
import Gramatyka.AbsLatte
import Control.Monad.State
import Control.Monad.Reader
import MonadUtils
import Gramatyka.WeakEq
import ErrorMessages
type CompilerGen s e result = StateT s (Reader e) result
type Compiler a = CompilerGen CompilerState Env a

debug :: Bool
debug = True

compile :: Tree a -> Compiler Env
compile t = do
  when debug (modify $ add $ Comment (show t))
  case t of
    Prgm z -> do
      mapM_ compile z
      ask
    FnDef _t ident _as blk -> do
      modify (add $ Function ident)
      modify (add $ Push EBP)
      modify (add $ Mov EBP ESP)
      modify $ add $ Comment "koniec preambuly"
      _ <- local (newEnv t) (compile blk)
      _ <- compile VRet
      ask
    Blk stmts -> do
      e <- ask
      _ <- chainReader compile stmts
      return e
    Empty -> ask
    BStmt b -> do
      e <- ask
      _ <- compile b
      return e
    Decl _t [] -> ask
    Decl typ ((Init ident expr):items) -> do
      (e', loc) <- addNewVar ident
      local (const e') (compileExpr expr)
      let reg = EAX
      modify $ add $ Pop reg
      modify $ add $ Set loc reg
      local (const e') (compile $ Decl typ items)
    Ass lv e2 -> do
      locLV <- getLValueLoc lv
      compileExpr e2
      let reg = EAX
      modify $ add $ Pop reg
      modify $ add $ Set locLV reg
      ask
    Ret e -> do
      compileExpr e
      modify $ add $ Pop EAX
      modify $ add $ Mov ESP EBP
      modify $ add $ Pop EBP
      modify $ add $ IRet
      ask
    VRet -> do
      modify $ add $ Mov ESP EBP
      modify $ add $ Pop EBP
      modify $ add $ IRet
      ask
    CondElse e b1 b2 -> do
      compileExpr e
      let reg = ECX
      l2 <- getNewLabel           -- else
      l3 <- getNewLabel           -- after if
      modify $ add $ Pop reg
      modify $ add $ JECXZ l2
      _ <- compile b1
      modify (add $ JMP l3)
      modify (add $ Label l2)
      _ <- compile b2
      modify (add $ Label l3)
      ask
    While e b -> do
      l1 <- getNewLabel
      l2 <- getNewLabel
      modify (add $ Label l1)
      compileExpr e
      let reg = ECX
      modify (add $ Pop reg)
      modify (add $ JECXZ l2)
      _ <- compile b
      modify $ add $ JMP l1
      modify $ add $ Label l2
      ask
    SExp e -> do
      compileExpr e
      ask

    -- compile it in other function
    TECast _ _ -> error "this should be compiled in compileExpr"
    TELValue _ _ -> error "this should be compiled in compileExpr"
    TELitInt _ _ -> error "this should be compiled in compileExpr"
    TELitTrue _ -> error "this should be compiled in compileExpr"
    TEConstr _ _ -> error "this should be compiled in compileExpr"
    TELitFalse _ -> error "this should be compiled in compileExpr"
    TEMethApp _ _ _ _ -> error "this should be compiled in compileExpr"
    TEAdd _ _ _ _ -> error "this should be compiled in compileExpr"
    TEString _ _ -> error "this should be compiled in compileExpr"
    TNeg _ _ -> error "this should be compiled in compileExpr"
    TNot _ _ -> error "this should be compiled in compileExpr"
    TEMul _ _ _ _ -> error "this should be compiled in compileExpr"
    TEOr _ _ _ -> error "this should be compiled in compileExpr"
    TEApp _ _ _ -> error "this should be compiled in compileExpr"
    TERel _ _ _ _ -> error "this should be compiled in compileExpr"
    TEAnd _ _ _ -> error "this should be compiled in compileExpr"


    Int -> error "Type should not be compiled"
    Str -> error "Type should not be compiled"
    Bool -> error "Type should not be compiled"
    ArrType _ -> error "Type should not be compiled"
    IdentType _ -> error "Type should not be compiled"
    Void -> error "Type should not be compiled"

    -- should be handled together with higher order structures
    Init _ _-> error "this shouldn't be handled seperately"
    Ident _-> error "Init should not be compiled seperately"
    Plus -> error "Init should not be compiled seperately"
    Minus -> error "Init should not be compiled seperately"
    Times -> error "Init should not be compiled seperately"
    Div -> error "Init should not be compiled seperately"
    Mod -> error "Init should not be compiled seperately"
    GTH -> error "Init should not be compiled seperately"
    GE -> error "Init should not be compiled seperately"
    EQU -> error "Init should not be compiled seperately"
    LVIdent _-> error "LVIdent should not be compiled seperately"
    Argument _ _ -> error "Argument should not be compiled seperately"

    ECast _-> error "expressions without type should not appear here"
    ELValue _-> error "expressions without type should not appear here"
    EConstr _-> error "expressions without type should not appear here"
    ELitInt _-> error "expressions without type should not appear here"
    ELitTrue -> error "expressions without type should not appear here"
    ELitFalse -> error "expressions without type should not appear here"
    EMethApp _ _ _-> error "expressions without type should not appear here"
    EApp _ _-> error "expressions without type should not appear here"
    EString _ -> error "expressions without type should not appear here"
    Neg _ -> error "expressions without type should not appear here"
    Not _ -> error "expressions without type should not appear here"
    EMul _ _ _ -> error "expressions without type should not appear here"
    EAdd _ _ _ -> error "expressions without type should not appear here"
    ERel _ _ _ -> error "expressions without type should not appear here"
    EOr _ _ -> error "expressions without type should not appear here"
    EAnd _ _ -> error "expressions without type should not appear here"

    -- desugared
    NoInit _ -> error "this should be desugared"
    Incr _ -> error "this should be desugared"
    Decr _ -> error "this should be desugared"
    Cond _ _ -> error "this should be desugared"
    NE -> error "this should be desugared"
    LE -> error "this should be desugared"
    LTH -> error "this should be desugared"
    Decl _ ((NoInit _):_) -> error "this should be desugared"
    -- NIY
    LVMember _ _ -> error "NIY"
    LVArrItem _ _ -> error "NIY"
    ClsDef _ _ -> error "NIY"
    ClsExtDef _ _ _ -> error "NIY"
    MemberDef _ _ -> error "NIY"
    MethodDef _ _ _ _ -> error "NIY"

addNewVar :: Ident -> Compiler (Env, MemLoc)
addNewVar i = do
  e <- ask
  let e' = newVar i e
  modify (add $ AddInt ESP (-1))
  return (e', varLoc i e')

compileExpr :: Expr -> Compiler ()
compileExpr expression = do
  when (debug) (modify $ add $ Comment (show expression))
  case expression of
    ECast _-> error "expressions without type should not appear here"
    ELValue _-> error "expressions without type should not appear here"
    EConstr _-> error "expressions without type should not appear here"
    ELitInt _-> error "expressions without type should not appear here"
    ELitTrue -> error "expressions without type should not appear here"
    ELitFalse -> error "expressions without type should not appear here"
    EMethApp _ _ _-> error "expressions without type should not appear here"
    EApp _ _-> error "expressions without type should not appear here"
    EString _ -> error "expressions without type should not appear here"
    Neg _ -> error "expressions without type should not appear here"
    Not _ -> error "expressions without type should not appear here"
    EMul _ _ _ -> error "expressions without type should not appear here"
    EAdd _ _ _ -> error "expressions without type should not appear here"
    ERel _ _ _ -> error "expressions without type should not appear here"
    EOr _ _ -> error "expressions without type should not appear here"
    EAnd _ _ -> error "expressions without type should not appear here"

    TECast _ _ -> error "NIY"
    TELValue _t lv -> case lv of
      LVIdent i -> do
        loc <- asks $ varLoc i
        modify $ add $ Get EAX loc
        -- case loc of
        --   (ML None a) ->
          --     Get EAX loc
        --     Push EAX
        -- putMemLockOnStack loc
        -- modify $ add $ Get EAX (ML ESP 1)
        modify $ add $ Push EAX
      _ -> error "NIY"
    TELitInt _t val -> do
      modify $ add $ IConst val
    TELitTrue _t -> do
      modify $ add $ IConst 1
    TEConstr _ _ -> error "NIY"
    TELitFalse _t -> do
      modify $ add $ IConst 0
    TEMethApp _ _ _ _ -> error "NIY"
    TEAdd t e1 op e2 -> do
      compileExpr e2
      compileExpr e1
      case op of
        Plus -> do
          case t of
            Int -> do
              let (r1, r2) = (EAX, ECX)
              modify $ add $ Pop r1
              modify $ add $ Pop r2
              modify $ add $ Add r1 r2
              modify $ add $ Push r1
            Str -> do
              modify $ add $ Call $ Ident "concat_strings"
              modify $ add $ AddInt ESP 2
              modify $ add $ Push $ EAX
            _ -> error "this type can't be here"
        Minus -> error "should be desugared"
    TEString _t str -> do
      lab <- getStringLabel str
      modify $ add $ PushStringLabel lab
    TNeg _t e -> do
      compileExpr e
      let reg = EAX
      modify $ add $ Pop reg
      modify $ add $ TwoCompNegation reg
      modify $ add $ Push reg
    TNot _t e -> do
      compileExpr e
      let reg = EAX
      modify $ add $ Pop reg
      modify $ add $ OneCompNegation reg
      modify $ add $ Push reg
    TEMul _t e1 op e2 -> case op of
      Times -> do
        compileExpr e1
        compileExpr e2
        let reg1 = EAX
        let reg2 = ECX
        modify $ add $ Pop reg2
        modify $ add $ Pop reg1
        modify $ add $ Mul reg1 reg2
        modify $ add $ Push reg1
      Div -> do
        compileExpr e1
        compileExpr e2
        let reg1 = EAX
        let reg2 = ECX
        modify $ add $ Pop reg2
        modify $ add $ Pop reg1
        modify $ add $ Cdq
        modify $ add $ IDiv reg2
        modify $ add $ Push reg1
      Mod -> do
        compileExpr e1
        compileExpr e2
        let reg1 = EAX
        let reg2 = ECX
        modify $ add $ Pop reg2
        modify $ add $ Pop reg1
        modify $ add $ Cdq
        modify $ add $ IDiv reg2
        modify $ add $ Push EDX
    TEOr _t e1 e2 -> do
      compileExpr e1
      compileExpr e2
      let reg1 = EAX
      let reg2 = ECX
      modify $ add $ Pop reg2
      modify $ add $ Pop reg1
      modify $ add $ Or reg1 reg2
      modify $ add $ Push reg1
    TEApp t ident args -> do
      mapM_ compileExpr $ L.reverse args
      modify $ add $ Call ident
      if (L.length args > 0)
        then (modify $ add $ AddInt ESP (toInteger $ L.length args))
        else (modify $ add $ Comment "tu powinno byc usuwanie argumentow ze stosu ale funkcja nie brala zadnych argumentow.")
      when (t /= Void) (modify $ add $ Push EAX)
    TERel t e1 relOp e2 -> do
      compileExpr e2
      compileExpr e1
      case t of
        Str -> do
          modify $ add $ Call $ Ident "streq"
        _ -> do
          let reg1 = EAX
          let reg2 = ECX
          modify $ add $ Pop reg1
          modify $ add $ Pop reg2
          modify $ add $ Cmp reg1 reg2
          modify $ add $ PutConst reg1 0
          modify $ add $ PutConst reg2 1
          case relOp of
            GE -> do
              modify $ add $ Cmovge reg1 reg2
            GTH -> do
              modify $ add $ Cmovg reg1 reg2
            EQU -> do
              modify $ add $ Cmove reg1 reg2
            _ -> error $ "if not matched here, then syntactic desugaring is wrong "
                 +++ expression
          modify $ add $ Push reg1
    TEAnd _t e1 e2 -> do
      compileExpr e1
      compileExpr e2
      let reg1 = EAX
      let reg2 = ECX
      modify $ add $ Pop reg2
      modify $ add $ Pop reg1
      modify $ add $ And reg1 reg2
      modify $ add $ Push reg1


getLValueLoc :: LValue -> Compiler MemLoc
getLValueLoc lv = case lv of
  LVIdent i -> asks $ varLoc i
  _ -> error "NIY other lvalues than just ident"

getNewLabel :: Compiler Label
getNewLabel = do
  labelId <- gets nextId
  modify (\s -> s { nextId = (1 :: Label) + nextId s } )
  return labelId

moveStack :: Integer -> Compiler Env
moveStack a = do
  modify $ add $ AddInt ESP a
  asks (changeStackSize a)

getStringLabel :: String -> Compiler Label
getStringLabel s = do
  z <- gets $ (M.lookup s) . strings
  if (isJust z)
    then (return $ fromJust z)
    else allocNew
  where
    allocNew = do
      lab <- gets nextId
      modify $ \x -> x { nextId = lab + 1 }
      modify $ add (s, lab)
      return lab

putMemLockOnStack :: MemLoc -> Compiler ()
putMemLockOnStack (ML None a) = do
  modify $ add $ IConst a
putMemLockOnStack (ML a b) = do
  let reg = EAX
  modify $ add $ Mov reg a
  modify $ add $ AddInt reg b
  modify $ add $ Push reg
