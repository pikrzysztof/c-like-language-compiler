{-# LANGUAGE GADTs, MultiParamTypeClasses,
TypeSynonymInstances, FlexibleInstances #-}
module ErrorMessages where

import TypeCheckerEnvironment
import MonadUtils
import Gramatyka.AbsLatte
import Data.List

infixr 6 +++

er :: String -> CE
er str = Err [str]

class Concatanable a b where
  (+++) :: a -> b -> String

instance {-# OVERLAPPING #-} Concatanable String String where
  (+++) = (++)

instance (Show a) => Concatanable a String where
   (+++) a b = (show a) +++ b

instance (Show a) => Concatanable String a where
  (+++) a b = a +++ (show b)

instance {-# OVERLAPPABLE #-} (Show a, Show b) => Concatanable a b where
  (+++) a b = (show a) +++ (show b)

missingFunctionError :: Expr -> CE
missingFunctionError fn = er $ "Nie znalazłem funkcji " +++ fn

incompatibleFn :: Function -> Function -> String
incompatibleFn fnCalled fnReal =
  "Wywołano funkcję której sygnatura powinna wyglądać tak: " +++ fnCalled +++
  " a faktyczna funkcja którą wołano wyglądała tak: " +++ fnReal


incompatibleReturnType :: Function -> Function -> CE
incompatibleReturnType fnNeeded fnReal =
  er $
  (incompatibleFn fnNeeded fnReal) +++ " (nie zgadza się typ wyniku funkcji)"

incompatibleArg :: Function -> Function -> Type -> Type -> CE
incompatibleArg fnNeeded fnReal argGiven argReal =
  er $
  (incompatibleFn fnNeeded fnReal) +++
  " (nie zgadza się typ argumentu funkcji, oczekiwano " +++
  argGiven +++ "a dano" +++ argReal +++ ")"

incompatibleArgNumber :: Function -> Function -> CE
incompatibleArgNumber fnCalled fnReal =
  er $ (incompatibleFn fnCalled fnReal) +++
        " (nie zgadza się liczba argumentów, oczekiwano " +++
        (length $ args fnReal) +++ " a dano " +++ (length $ args fnCalled) +++
        ")"

dummyE :: CE
dummyE = er $ "dummy"

declarationInWrongPlace :: Stmt -> CE
declarationInWrongPlace d@(Decl _ _)  =
  er $ "One can only declare a variable " +++
  "directly in a block, error with declaration " +++ d +++ "."
declarationInWrongPlace _otherStuff =
  error "Expected a declaration in 'declarationInWrongPlace' error."

noMainE :: CE
noMainE = er $ "No main function found."

wrongMainE :: CE
wrongMainE = er $ "Wrong main function signature."

getDups :: (Eq a, Eq b) => (a -> b) -> [a] -> [a]
getDups f as = as \\ (nubBy (\x y -> (f x) == (f y)) as)

getDupArgs :: Function -> [Argument]
getDupArgs f = filter ((flip elem repeated) . snd) as
  where
    as = (args f)
    names = (map snd as)
    repeated = names \\ nub names

duplicatedArg :: Function ->  CE
duplicatedArg f = er $ "Arguments " +++ getDupArgs f +++
                  " are duplicated in params list."

duplicateDecl :: Stmt -> CE
duplicateDecl (Decl t is) =
  er $ "Two arguments with the same name: " +++ t +++
         (getDups (\(Init ident _expr) -> ident) is) +++ "."
duplicateDecl e = error $ "duplicateDecl should not be called with " +++ e

assignTypeNoMatch :: Stmt -> CE
assignTypeNoMatch s =
  er $ "Left and right assignment types does not match in " +++ s +++ "."

unexpectedRetWithoutType :: Stmt -> CE
unexpectedRetWithoutType s =
  er $ "I have not expected this function to return a value in " +++ s +++ "."

wrongRetType :: Stmt -> Type -> Type-> CE
wrongRetType s should is =
  er $ "Wrong return type in " +++ s +++ " should be " +++
         should +++ "got " +++ is +++ "."

unexpectedRetWithType :: Stmt -> CE
unexpectedRetWithType s =
  er $ "I have not expected this function to return a value in " +++ s +++ "."

expectedBool :: Stmt -> CE
expectedBool s =
  er $
  "I have expected a condition to be a boolean expression in " +++ s ++ "."

notInitialized :: Expr -> CE
notInitialized e =
  er $ "Variable " +++ e +++ " has not been initialized."

outOfBounds :: Integer -> CE
outOfBounds i =
  er $ "Integer " +++ i +++
  " is out of bounds, expected to be within signed i32 range."

voidVar :: Stmt -> CE
voidVar t =
  er $ "You can't have a void variable in " +++ t +++ "."

incompatibleTypes :: Expr -> Function -> CE
incompatibleTypes f fenv =
  er $ "Funcion called as " +++ f +++ " has incompatible argument types with "
  +++ "declared function as " +++ fenv

negatingWrongType :: Expr -> Type -> CE
negatingWrongType e t =
  er $ "Trying to negate non-integer type in" +++ e +++ " which is of type "
  +++ t +++ "."

nottingWrongType :: Expr -> Type -> CE
nottingWrongType e t =
  er $ "Trying to NOT non-boolean type in " +++ e +++ " which is of type "
  +++ t +++ "."

cantMultiply :: Expr -> Type -> Type -> CE
cantMultiply ex t1 t2 =
  er $ "Cant do expr " +++ ex +++ " left side has type " +++ t1 +++
  " right side has type " +++ t2 +++ "."
