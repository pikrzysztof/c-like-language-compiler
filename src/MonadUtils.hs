{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs, GADTs #-}
module MonadUtils where

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Gramatyka.AbsLatte
import Gramatyka.PrintLatte

import Data.List (intersperse)
data CE = Err [String]
        deriving (Eq, Ord)

type ErrMonad m a = ExceptT CE m a
instance Show CE where
  show (Err []) = error "ktoś chce pokazać pusty błąd."
  show (Err x) = foldl (++) "ERROR\n" (reverse $ intersperse "\n" x)

infixr 0 &&&
(&&&) :: Monad m => String -> ErrMonad m a -> ErrMonad m a
(&&&) bl pc = catchE pc (\(Err bledy) -> throwE $ Err $ bl : bledy)

chainReader :: MonadReader b m =>  (a -> m b) -> [a] -> m b
chainReader _ [] = do
  ask

chainReader f (x:xy) = do
  tmp <- f x
  local (const tmp) (chainReader f xy)

class Testable a b c where
  test :: a -> Except b c

instance Testable (Bool, a) a () where
  test :: (Bool, a) -> Except a ()
  test (False, x) = throwE x
  test (True, _) = return ()

instance Testable (Maybe a) a () where
  test :: (Maybe a) -> Except a ()
  test (Just err) = throwE err
  test Nothing = return ()

instance Testable (Either a b) a b where
  test :: (Either a b) -> Except a b
  test (Left err) = throwE err
  test (Right good) = return good

instance Testable (Maybe a, a -> b) b () where
  test (Nothing, _) = return ()
  test (Just a, f) = throwE (f a)

chainReaderMap :: MonadReader b m => (a -> m (a, b)) -> [a] -> m ([a], b)
chainReaderMap _ [] = do
  e <- ask
  return ([], e)

chainReaderMap f (x:xs) = do
  (r, e) <- f x
  (rest, e') <- local (const e) (chainReaderMap f xs)
  return (r:rest, e')

class Print2 a where
  print2Tree :: a -> String


instance Print2 (Tree a) where
  print2Tree x = case x of
    Prgm _ -> printTree x
    FnDef _ _ _ _ -> printTree x
    Argument _ _ -> printTree x
    Blk _ -> printTree x
    Empty -> printTree x
    BStmt _ -> printTree x
    Decl _ _ -> printTree x
    Ass _ _ -> printTree x
    Ret _ -> printTree x
    VRet -> printTree x
    CondElse _ _ _ -> printTree x
    While _ _ -> printTree x
    SExp _ -> printTree x
    Init _ _ -> printTree x
    Int -> printTree x
    Str -> printTree x
    Bool -> printTree x
    IdentType _-> printTree x
    Void -> printTree x
    Fun _ _ -> printTree x
    LVIdent _ -> printTree x
    TECast _ _ -> printTree x
    TELValue _ _ -> printTree x
    TEConstr _ _ -> printTree x
    TELitInt _ _ -> printTree x
    TELitTrue _ -> printTree x
    TELitFalse _ -> printTree x
    TEMethApp _ _ _ _ -> printTree x
    TEApp _ _ _ -> printTree x
    TEString _ _ -> printTree x
    TNeg _ _ -> printTree x
    TEMul _ _ _ _ -> printTree x
    TEAdd _ _ _ _ -> printTree x
    TERel _ _ _ _ -> printTree x
    TEAnd _ _ _ -> printTree x
    TEOr _ _ _ -> printTree x
    Plus -> printTree x
    Times -> printTree x
    Div -> printTree x
    Mod -> printTree x
    GTH -> printTree x
    GE -> printTree x
    EQU -> printTree x
    Ident _ -> printTree x
    Incr _ -> printTree x
    Decr _ -> printTree x
    NoInit _ -> printTree x
    Minus -> printTree x
    LTH -> printTree x
    NE -> printTree x
    LE -> printTree x
    Cond _ _ -> printTree x
    ECast _ -> printTree x
    ELValue _ -> printTree x
    EConstr _ -> printTree x
    ELitInt _ -> printTree x
    ELitTrue -> printTree x
    ELitFalse -> printTree x
    EMethApp _ _ _ -> printTree x
    EApp _ _ -> printTree x
    EString _ -> printTree x
    TNot _ _ -> printTree x
    Neg _ -> printTree x
    Not _ -> printTree x
    EMul _ _ _ -> printTree x
    EAdd _ _ _ -> printTree x
    ERel _ _ _ -> printTree x
    EAnd _ _ -> printTree x
    EOr _ _ -> printTree x
    LVMember _ _ -> printTree x
    LVArrItem _ _ -> printTree x
    ArrType _ -> printTree x
    ClsDef _ _ -> printTree x
    ClsExtDef _ _ _ -> printTree x
    MemberDef _ _ -> printTree x
    MethodDef _ _ _ _ -> printTree x
