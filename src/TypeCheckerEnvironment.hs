{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module TypeCheckerEnvironment where

import Prelude as P hiding (sum)
import Data.Map as M
import Gramatyka.AbsLatte
import Data.Tuple

type Var = (Ident, Type)
type FnMap = Map Ident Function
type ClsMap = Map Ident Class
type VarMap = Map Ident Type
type Argument = (Type, Ident)
              -- deriving (Eq, Ord, Show)

data Env = Env { variables :: VarMap,
                 functions :: FnMap,
                 classes :: ClsMap,
                 checkedFnType :: Maybe Type
               } deriving (Ord, Eq, Show)

envZero :: Env
envZero = Env { variables = M.empty,
                functions = M.empty,
                classes = M.empty,
                checkedFnType = Nothing
              }

data Function = Fn { fnName :: Ident,
                     args :: [Argument],
                     returnType :: Type
                   } deriving (Eq, Ord, Show)

data Class = Cz { clsName :: Ident,
                  memberVar :: VarMap,
                  superclass :: Maybe Ident
                 } deriving (Eq, Ord, Show)

mainFunction :: Function
mainFunction = Fn { fnName = Ident "main",
                    args = [],
                    returnType = Int
                    }

class NewEnv a where
  newEnv :: a -> Env -> Env

class Summable a where
  sum :: Env -> a -> Env

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
  giveSuperclass e (IdentType i) = giveSuperclass e i
  giveSuperclass _ (Fun _ _) = error "Ktoś chce superklasę funkcji."
  giveSuperclass _e _ = Nothing -- typ prosty
  superclasses e (IdentType i) = superclasses e i
  superclasses _ (Fun _ _) = error "Ktoś chcę listę superklas funkcji."
  superclasses _e _ = []        -- typ prosty lub void

instance EnvElement Type where
  add e ident type' = e { variables = M.insert ident type' (variables e)}
  geti e i = getGeneral e variables i
  geto e (IdentType o) = getGeneral e variables o
  geto _e (Fun _ _) = error "Zmienna która jest funkcją."
  geto _e Void = error "Zmienna typu void."
  geto _ _ = error $ "Nie wiem jak pobrać ze środowiska zmienną tylko na " ++
             "podstawie typu..."


instance EnvElement Class where
  add e ident cls' = e { classes = M.insert ident cls' (classes e)}
  geti e i = getGeneral e classes i
  geto e o = getGeneral e classes (clsName o)

instance EnvElement Function where
  add e ident fn' = e { functions = M.insert ident fn' (functions e)}
  geti e i = getGeneral e functions i
  geto e o = getGeneral e functions (fnName o)

getGeneral :: Ord a => Env -> (Env -> Map a b) -> a -> Maybe b
getGeneral env getter key = M.lookup key (getter env)

instance Summable FnMap where
  sum e m = e { functions = union (functions e) m }

instance Summable Function where
  sum e f = e { functions = insert (fnName f) f (functions e) }

instance NewEnv Function where
  newEnv f e = e { variables = fromList $ P.map swap (args f),
                   checkedFnType = Just $ returnType f}

instance Summable Var where
  sum e v = e { variables = M.insert (fst v) (snd v) (variables e)}

instance Summable Stmt where
  sum e (Decl t i) =
    P.foldr (flip sum) e (P.map
                          ((flip (,)) t)
                          (P.map (\(Init ident _) -> ident) i))
  sum _ _ = error "Can't sum other statement than Decl."
