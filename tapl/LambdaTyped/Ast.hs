{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module LambdaTyped.Ast(TypeF(..), TermF(..), Term, Type, Meta(..), Ann(..), pattern Un) where

data Meta = Meta {
    literal :: String
  , lin :: Int
  , col :: Int
  } deriving(Eq, Show)

data Ann f = Ann {
  meta :: Meta,
  unann :: f (Ann f)
}
deriving instance (Eq (f (Ann f))) => Eq (Ann f)
deriving instance (Show (f (Ann f))) => Show (Ann f)

pattern Un :: f (Ann f) -> Ann f
pattern Un a <- Ann _ a

data TypeF f
  = TyBool
  | TyUnit
  | TyNat 
  | TyProd f f
  | TyFunc f f
  | TyAny
  deriving(Show, Eq, Functor)
type Type = Ann TypeF

data TermF f
  = TT
  | FF
  | UU
  | Numb Int
  | Abs String Type f
  | App f f
  | Let String f f
  | Pair f f
  | Fst f 
  | Snd f 
  | If f f f
  | Var String
  | Sum f f
  | Mult f f
  | IsZero f
  deriving(Eq, Show, Functor)
type Term = Ann TermF
