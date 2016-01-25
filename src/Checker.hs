{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE GADTs #-}
module Checker where

import Prelude hiding(sum)
import Data.Int
-- Insignificant (library) imports
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.List hiding (sum)
import Gramatyka.AbsLatte
import Data.Maybe as Mb
import Data.Either

import ASTAdapter
import MonadUtils
import TypeCheckerEnvironment as TCE
import ErrorMessages

type Validator a = ExceptT CE (Reader Env) a

odpal :: Validator a -> Env -> Either CE a
odpal r e0 = runIdentity $ runReaderT (runExceptT r) e0

checkTree :: Program -> Except CE Program
checkTree tr = do
  () <- invalidDecl False tr
  let tr' = desugar tr
  checkTypesWithFunctions tr'

checkExistence :: (Ord a) => (M.Map a b) -> (z -> a) -> z -> Bool
checkExistence m f k = M.member (f k) m

dups :: (Eq b)  => (a -> b) -> [a] -> Bool
dups f as = null $ a' \\ (nub a')
  where
    a' = map f as

checkTypes :: Tree a -> Validator (Tree a, Env)
checkTypes p = do
  (show p) &&& case p of
    FnDef t i as b -> do
      let fn = fromJust $ adapt p
      unless
        (dups snd (args fn))
        (throwE $ duplicatedArg fn)
      (b', _e) <- local (newEnv fn) (checkTypes b)
      newT $ FnDef t i as b'
    Blk stmts -> do
      (stmts', _e) <- chainReaderMap checkTypes stmts
      newT $ Blk stmts'
    Empty -> standard
    BStmt stmts -> do
      (stmts', _e) <- checkTypes stmts
      newT $ BStmt stmts'
    Decl t items -> do
      when
        (t == Void)
        (throwE $ voidVar p)
      unless
        (dups id items)
        (throwE $ duplicateDecl p)
      e <- asks ((flip sum) p)
      newE e
    Ass e1 e2 -> do
      (tr1, ty1) <- checkExprType e1
      (tr2, ty2) <- checkExprType e2
      e <- ask
      unless (isSuperclass e ty2 ty1) (throwE $ assignTypeNoMatch p)
      newT $ Ass tr1 tr2
    Ret e -> do
      (tr, ty) <- checkExprType e
      z <- asks checkedFnType
      e' <- ask
      when (isNothing z) (throwE $ unexpectedRetWithType p)
      unless
        (isSuperclass e' ty (fromJust z))
        (throwE $ wrongRetType p (fromJust z) ty)
      newT $ Ret tr
    VRet -> do
      rt <- asks checkedFnType
      when (isJust rt) (throwE $ unexpectedRetWithType p)
      standard
    CondElse e b1 b2 -> do
      (tr, ty) <- checkExprType e
      when (ty /= Bool) (throwE $ expectedBool p)
      (tr1, _e) <- checkTypes b1
      (tr2, _e) <- checkTypes b2
      newT (CondElse tr tr1 tr2)
    While e b -> do
      (tr1, ty1) <- checkExprType e
      when (ty1 /= Bool) (throwE $ expectedBool p)
      (tr, _e) <- checkTypes b
      newT $ While tr1 tr
    SExp e -> do
      (tr1, _t1) <- checkExprType e
      newT $ SExp tr1
    _ ->
      standard
  where
    standard = standard' p
    standard' :: Tree a -> Validator (Tree a, Env)
    standard' pp = do
      e <- ask
      return  (pp, e)
    newT :: Tree a -> Validator (Tree a, Env)
    newT t = do
      e <- ask
      return (t, e)
    newE = newE' p
    newE' :: Tree a -> Env -> Validator (Tree a, Env)
    newE' pp e = return (pp, e)
    --new :: Tree a -> Env -> Validator (Tree a, Env)
    --new t e = return (t, e)



checkTypesWithFunctions :: Program -> Except CE (Program)
checkTypesWithFunctions tr@(Prgm td) = do
  let allFns = Mb.mapMaybe (adapt :: TopDef -> Maybe Function) td
  let allFnsMap = M.fromList $ zip (map fnName allFns) allFns
  let env = (TCE.sum :: Env -> FnMap -> Env) envZero allFnsMap
  _ <- checkMain env
  (trNew, _env) <- except $ odpal (checkTypes tr) env
  return trNew

checkMain :: Env -> Except CE ()
checkMain e = do
  case geto e mainFunction of
    Nothing -> throwE noMainE
    Just fn ->
      if (fn == mainFunction)
      then (return ())
      else (throwE wrongMainE)

-- t1 >= t2, i.e. Integer >= Int
isSuperclass :: Env -> Type -> Type -> Bool
isSuperclass e t1@(IdentType _) t2 = t1 `elem` t2:(map
                                                   (IdentType . clsName)
                                                   (superclasses e t2))
isSuperclass _e t1 t2 = t1 == t2

desugar :: Tree a -> Tree a
desugar t = case t of
  Cond e s -> CondElse e s Empty
  Incr a -> Ass a (EAdd a Plus (ELitInt 1))
  Decr a -> Ass a (EAdd a Minus (ELitInt 1))
  Decl type' stuff -> Decl type' (map (makeInit type') stuff)
  -- tu mozna jeszcze dodac taki motyw zeby generowac kod skaczacy od razu...
  _ -> composOp desugar t
  where
    makeInit :: Type -> Item -> Item
    makeInit type' (NoInit a) = Init a (defaultValue type')
    makeInit _ tmp@(Init _ident _expr) = tmp
    defaultValue :: Type -> Expr
    defaultValue type' = case type' of
      Str -> EString ""
      Int -> ELitInt 0
      Bool -> ELitFalse
      IdentType ident -> EConstr ident
      _ -> error "I have not expected void or function type here."

invalidDecl :: Bool -> Tree a -> Except CE ()
invalidDecl b t = case t of
  dec@(Decl _ _) -> unless b (throwE $ declarationInWrongPlace dec)
  blk@(Blk _stmts) -> composOpM_ (invalidDecl True) blk
  whatever -> composOpM_ (invalidDecl False) whatever

checkExprType :: Expr -> Validator (Expr, Type)
checkExprType ex = do
  (show ex) &&& case ex of
    EVar ident -> do
      t <- asks ((M.lookup ident) . variables)
      when (isNothing t) (throwE $ notInitialized ex)
      let t' = fromJust t
      return $ std t' --(addTe t', t')
    ELitInt i -> do
      when (i > (toInteger (maxBound :: Int32)) ||
            i < (toInteger (minBound :: Int32))) (throwE $ outOfBounds i)
      return $ (addTe Int, Int)
    ELitTrue -> return $ std Bool
    ELitFalse -> return $ std Bool
    EString _ -> return $ std Str
    ECast _ -> return $ std Null
    EMember _ _ -> error "NIY"
    EConstr _ -> error "NIY"
    EMethApp _ _ _ -> error "NIY"
    EApp ident es -> do
      mf <- asks ((M.lookup ident) . functions)
      when (isNothing mf) (throwE $ missingFunctionError ex)
      let f = fromJust mf
      ts <- mapM checkExprType es
      when ((length . args) f /= length es)
        (throwE $ incompatibleArgNumber (fromJust $ adapt ex) f)

      -- moze trzeba odwrocic kolejnosc argumentów?????????
      env <- ask
      unless
        (all
         (uncurry (isSuperclass env))
         (zip
          (map snd ts)
          (map fst (args f))))
        (throwE $ incompatibleTypes ex f)
      let rt = returnType f
      return (TEApp rt ident (map fst ts), rt)
    Neg e -> do
      (e', t) <- checkExprType e
      when
        (t /= Int)
        (throwE $ negatingWrongType ex t)
      return (TNeg t e', t)
    Not e -> do
      (e', t) <- checkExprType e
      when
        (t /= Bool)
        (throwE $ nottingWrongType ex t)
      return (TNot t e', t)
    EMul e1 op e2 -> do
      checkT Int ex e1 e2 (\t e1' e2' -> TEMul t e1' op e2')
    EAdd e1 op e2 -> do
      checkT Int ex e1 e2 (\t e1' e2' -> TEAdd t e1' op e2')
    ERel e1 op e2 -> do
      case op of
        EQU -> do
          (e1', t1) <- checkExprType e1
          (e2', t2) <- checkExprType e2
          when
            (t1 /= t2)
            (throwE $ cantMultiply ex t1 t2)
          return $ (TERel t1 e1' op e2', t1)
        NE -> do
          (e1', t1) <- checkExprType e1
          (e2', t2) <- checkExprType e2
          when
            (t1 /= t2)
            (throwE $ cantMultiply ex t1 t2)
          return $ (TERel t1 e1' op e2', t1)
        _ -> do
          (e1', t1) <- checkExprType e1
          (e2', t2) <- checkExprType e2
          when (t1 /= t2 || t1 /= Int) (throwE $ cantMultiply ex t1 t2)
          return $ (TERel t1 e1' op e2', t1)
    EAnd e1 e2 -> do
      (e1', t1) <- checkExprType e1
      (e2', t2) <- checkExprType e2
      when (t1 /= t2 || t1 /= Bool) (throwE $ cantMultiply ex t1 t2)
      return (TEAnd t1 e1' e2', t1)
    EOr e1 e2 -> do
      (e1', t1) <- checkExprType e1
      (e2', t2) <- checkExprType e2
      when (t1 /= t2 || t1 /= Bool) (throwE $ cantMultiply ex t1 t2)
      return (TEOr t1 e1' e2', t1)
    _ -> error "One shouldn't check TE_ nodes."
  where
    checkT :: Type -> Expr -> Expr -> Expr ->
              (Type -> Expr -> Expr -> Expr) ->
              Validator (Expr, Type)
    checkT typ pretty e1 e2 constr = do
      (e1', t1) <- checkExprType e1
      (e2', t2) <- checkExprType e2
      when
        (t1 /= t2 || t1 /= typ)
        (throwE $ cantMultiply pretty t1 t2)
      return $ (constr t1 e1' e2', t1)
    --supercls = ask >>= isSuperclass
    std x = (addTe x, x)
    addTe = addT ex
    addT exx t = addT' exx t
    addT' :: Expr -> Type -> Expr
    addT' exx t = case exx of
      ECast i -> TECast t i
      EVar i -> TEVar t i
      EMember e i -> TEMember t e i
      EConstr i -> TEConstr t i
      ELitInt i -> TELitInt t i
      ELitTrue -> TELitTrue t
      ELitFalse -> TELitFalse t
      EMethApp e i es -> TEMethApp t e i es
      EApp i es -> TEApp t i es
      EString s -> TEString t s
      Neg e -> TNeg t e
      Not e -> TNot t e
      EMul e1 op e2 -> TEMul t e1 op e2
      EAdd e1 op e2 -> TEAdd t e1 op e2
      ERel e1 op e2 -> TERel t e1 op e2
      EAnd e1 e2 -> TEAnd t e1 e2
      EOr e1 e2 -> TEOr t e1 e2
      -- te juz maja typy, to jest rodzina T*
      x -> x
