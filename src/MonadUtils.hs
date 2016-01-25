{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
module MonadUtils where

import Control.Monad.Trans.Except
import Control.Monad.Reader
--import Gramatyka.AbsLatte
--import Gramatyka.ComposOp
data CE = Err [String]
        deriving (Eq, Ord)

type ErrMonad m a = ExceptT CE m a
instance Show CE where
  show (Err []) = error "ktoś chce pokazać pusty błąd."
  show (Err x) = foldl (++) "ERROR\n" x

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
