{-# LANGUAGE GADTs #-}
module CompilerEnv where

import Gramatyka.AbsLatte
import qualified Data.Map as M
import CompilerState
import Data.Maybe (fromJust)

data Env = Env { variables :: M.Map Ident MemLoc,
                 sp :: Integer
               } deriving (Eq, Ord, Show)

envZero :: Env
envZero = Env { variables = M.empty,
                sp = 0
              }

newEnv :: TopDef -> Env -> Env
newEnv (FnDef _t _ident as _blk) e =
  e { variables =
        M.fromList (
        zip
        (reverse $ map (\(Argument _t i) -> i) as)
        (map (\x -> ML EBP x) [2, 3 ..])),
      sp = 0
    }
newEnv _ _ = error "classes NIY"

newVar :: Ident -> Env -> Env
newVar i e = Env { variables = M.insert i (ML EBP sp') (variables e),
                   sp = sp'
                 }
  where
    sp' = (sp e) - 1

-- should not error, as the code is checked
varLoc :: Ident -> Env -> MemLoc
varLoc i e = fromJust $ M.lookup i (variables e)

changeStackSize :: Integer -> Env -> Env
changeStackSize i e = e { sp = i + sp e }
