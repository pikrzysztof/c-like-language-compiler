{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             InstanceSigs #-}
module ASTAdapter where

import TypeCheckerEnvironment as TCE
import Gramatyka.AbsLatte as Abs

class Adaptable a b where
  adapt :: a -> Maybe b

instance Adaptable (Abs.Tree a) TCE.Function where
  adapt :: Abs.Tree a -> Maybe TCE.Function
  adapt (FnDef type' ident args' _blk) =
    Just $
    TCE.Fn { fnName = ident,
             args = map (\(Abs.Argument t ident') -> (t, ident')) args',
             returnType = type'
           }
  adapt _ = Nothing

instance Adaptable (Abs.Tree a) Type where
  adapt :: Abs.Tree a -> Maybe Abs.Type
  adapt Int = Just Int
  adapt Void = Just Void
  adapt Str = Just Str
  adapt t@(IdentType _) = Just t
  adapt _ = Nothing
