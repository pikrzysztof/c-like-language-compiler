{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module TypeCheckerEnvironment where

import Data.Map as Map
import Gramatyka.AbsLatte

data Env = Env { variables :: Map Ident Type,
                 functions :: Map Ident Function,
                 classes :: Map Ident Class,
                 validateFunctions :: [Function],
                 validateTypeExistence :: [Type]
               } deriving (Ord, Eq, Show)

envZero :: Env
envZero = Env { variables = Map.empty,
                functions = Map.empty,
                classes = Map.empty,
                validateFunctions = [mainFunction],
                validateTypeExistence = []
              }

data Function = Fn { fnName :: Ident,
                     args :: [Type],
                     returnType :: Type
                   } deriving (Eq, Ord, Show)

data Class = Cz { clsName :: Ident,
                  memberVar :: Map Ident Type,
                  superclass :: Maybe Ident
                 } deriving (Eq, Ord, Show)

mainFunction :: Function
mainFunction = Fn { fnName = Ident "main",
                    args = [],
                    returnType = IdentType $ Ident "int"
                    }


class EnvElement a where
  add :: Env -> Ident -> a -> Env
  geti :: Env -> Ident -> Maybe a
  geto :: Env -> a -> Maybe a

class HasSuperclass a where
  giveSuperclass :: Env -> a -> Maybe Class
  superclasses :: Env -> a -> [Class]


instance HasSuperclass Ident where
  giveSuperclass e i = (geti :: Env -> Ident -> Maybe Class) e i
                       >>= superclass
                       >>= (geti :: Env -> Ident -> Maybe Class) e
  superclasses e i = superClassesMaybe e (geti e i)
    where
      superClassesMaybe :: Env -> Maybe Class -> [Class]
      superClassesMaybe _env Nothing = []
      superClassesMaybe env (Just cls) = cls:(superClassesMaybe
                                              env
                                              (giveSuperclass
                                               env
                                               (clsName cls)))
instance HasSuperclass Class where
  giveSuperclass e c = giveSuperclass e (clsName c)
  superclasses e c = superclasses e (clsName c)
instance HasSuperclass Type where
  giveSuperclass _e Void = Nothing
  giveSuperclass e (IdentType i) = giveSuperclass e i
  giveSuperclass _ (Fun _ _) = error "Ktoś chce superklasę funkcji."
  superclasses _e Void = []
  superclasses e (IdentType i) = superclasses e i
  superclasses _ (Fun _ _) = error "Ktoś chcę listę superklas funkcji."

instance EnvElement Type where
  add e ident type' = e { variables = Map.insert ident type' (variables e)}
  geti e i = getGeneral e variables i
  geto e (IdentType o) = getGeneral e variables o
  geto _e (Fun _ _) = error "Zmienna która jest funkcją."
  geto _e Void = error "Zmienna typu void."

instance EnvElement Class where
  add e ident cls' = e { classes = Map.insert ident cls' (classes e)}
  geti e i = getGeneral e classes i
  geto e o = getGeneral e classes (clsName o)

instance EnvElement Function where
  add e ident fn' = e { functions = Map.insert ident fn' (functions e)}
  geti e i = getGeneral e functions i
  geto e o = getGeneral e functions (fnName o)

getGeneral :: Ord a => Env -> (Env -> Map a b) -> a -> Maybe b
getGeneral env getter key = Map.lookup key (getter env)
