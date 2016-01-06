{-# LANGUAGE GADTs #-}
module Checker where

-- Insignificant (library) imports
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.List
import Gramatyka.AbsLatte
import Data.Maybe
import Data.Either

-- Local, significant imports
import MonadUtils
import TypeCheckerEnvironment as TCE
import ErrorMessages

type Validator a = ExceptT CE (Reader Env) a

checkTree :: Tree a -> Validator Env
checkTree a = "Error in " ++ (show a) &&& case a of
  Prgm declList -> do
    env <- chainReader checkTree declList
    let lista = lefts $ map (checkFunction env) (validateFunctions benv)
    let element = find (const True)
    -- errs <- (mapM) (checkFunction env) (map Right $ validateFunctions env)
    return env
    -- let bad = find isJust (map
    --                        (checkFunction env)
    --                        (TCE.validateFunctions env)) in do
    --   case bad of
    --     Nothing ->
    --       return env
    --     Just (Just fnErr) ->
    --       throwE fnErr
    --     _ -> error "błąd w CheckTree, isJust dało prawdę przy Nothing"
  FnDef _ret _ident _args _block -> undefined
  _ -> undefined

checkFunction :: Env -> TCE.Function -> Except CE ()
checkFunction e f = do
  let fenv = TCE.geto e f
  -- function existence
  _ <- (test :: (Bool, a) -> Except a ())
       (isNothing fenv, missingFunctionError f)
  let retType = returnType f
  -- args list are of equal length
  _ <- (test :: (Bool, a) -> Except a ())
       (((==) (length $ args $ fromJust fenv) (length $ args f)),
        incompatibleArgNumber f (fromJust fenv))
  -- return type is compatible
  _ <- (test :: (Bool, a) -> Except a ())
       (retType == Void || isSuperclass e retType (returnType $ fromJust fenv),
        incompatibleReturnType f (fromJust fenv))
  -- args list have compatible types
  -- _ <- (test :: )
  return ()




-- może dać błąd
-- checkFunction :: Env -> TCE.Function -> Maybe CE
-- checkFunction e f = let fe = TCE.geto e f in
--   -- case fe of
--   -- Nothing -> Just $ missingFunctionError f
--   Just fReal -> let badArg =
--                       find
--                       (not . uncurry $ isSuperclass e)
--                       (zip (args f) (args fReal))
--                 in
--                   case badArg of
--                   Nothing -> case returnType f of
--                     Void ->
--                       Nothing
--                     someType -> if (isSuperclass
--                                     e
--                                     (returnType fReal)
--                                     someType) then
--                                   Nothing
--                                 else
--                                   Just $ incompatibleReturnType f fReal
--                   Just (argCalled, argReal) ->
--                     Just $ incompatibleArg f fReal argCalled argReal

-- t1 >= t2, i.e. Integer >= Int
isSuperclass :: Env -> Type -> Type -> Bool
isSuperclass _e Void t2 = Void == t2
isSuperclass e t1 t2 = t1 `elem` t2:(map
                                      (IdentType . clsName)
                                      (superclasses e t2))
