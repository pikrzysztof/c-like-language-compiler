module ErrorMessages where

import TypeCheckerEnvironment
import MonadUtils
import Gramatyka.AbsLatte

missingFunctionError :: Function -> CE
missingFunctionError fn = Err $ ["Nie znalazłem funkcji " ++ show fn]

incompatibleFn :: Function -> Function -> String
incompatibleFn fnCalled fnReal =
  "Wywołano funkcję której sygnatura powinna wyglądać tak: " ++
  (show fnCalled) ++
  " a faktyczna funkcja którą wołano wyglądała tak: " ++
  (show fnReal)


incompatibleReturnType :: Function -> Function -> CE
incompatibleReturnType fnNeeded fnReal =
  Err $
  [ (incompatibleFn fnNeeded fnReal) ++
    " (nie zgadza się typ wyniku funkcji)"]

incompatibleArg :: Function -> Function -> Type -> Type -> CE
incompatibleArg fnNeeded fnReal argGiven argReal =
  Err $
  [ (incompatibleFn fnNeeded fnReal) ++
    " (nie zgadza się typ argumentu funkcji, oczekiwano " ++
    (show argGiven) ++
    "a dano" ++
    (show argReal) ++
    ")"]

incompatibleArgNumber :: Function -> Function -> CE
incompatibleArgNumber fnCalled fnReal =
  Err $ [(incompatibleFn fnCalled fnReal) ++
        " (nie zgadza się liczba argumentów, oczekiwano " ++
        (show $ length $ args fnReal) ++
        " a dano " ++
        (show $ length $ args fnCalled) ++
        ")"]
