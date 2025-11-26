{-# LANGUAGE LambdaCase #-}
module LambdaTyped.Typechecker(typechecks) where

import LambdaTyped.Ast
import Control.Monad.RWS

type TyContext = [(String, Type)]

type Typechecker a = RWS TyContext [String] () a 

trow :: String -> Typechecker ()
trow s = tell [s]

trowIf :: Bool -> String -> Typechecker ()
trowIf True a = trow a
trowIf _ _ = pure ()

asAny :: Typechecker a -> Typechecker Type
asAny t = t >> pure TyAny

typechecks :: Term -> Type
typechecks term = 
  case evalRWS (typeof term) [] () of 
    (a, []) -> a
    (_, errs) -> error $ unlines errs

typeof :: Term -> Typechecker Type
typeof term =
  case term of
    TT -> pure TyBool
    FF -> pure TyBool
    UU -> pure TyUnit
    Numb _ -> pure TyNat
    If cnd t1 t2 -> do
      condt <- typeof cnd
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (condt /= TyBool) "if condition must be boolean"
      trowIf (t1t /= t2t) "if branches must evaluate to same type"
      typeof t2
    Var str -> do
      asks (lookup str) >>= \case
        Nothing -> asAny $ trow ("unknown variable " <> str)
        Just a -> return a 
    Abs str ty body -> TyFunc ty <$> local ((str,ty):) (typeof body)
    App t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      case t1t of
        TyFunc TyAny b -> pure b
        TyFunc a b -> trowIf (t2t /= a) "function paramether of wrong type"
                      >> pure b
        _ -> asAny $ trow "non-function being applied"
    Let str t1 t2 -> do
      t1t <- typeof t1
      local ((str, t1t):) (typeof t2)
    Pair t1 t2 -> (:*:) <$> typeof t1 <*> typeof t2
    Fst t1 -> do
      typeof t1 >>= \case
        a :*: _ -> pure a
        _ -> asAny $ trow "non-pair type on fst call"
    Snd t1 -> do
      typeof t1 >>= \case
        _ :*: a -> pure a
        _ -> asAny $ trow "non-pair type on snd call"
    Sum t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (t1t /= TyNat || t2t /= TyNat) "sum of non-naturals"
      pure TyNat
    Mult t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (t1t /= TyNat || t2t /= TyNat) "mult of non-naturals"
      pure TyNat
    IsZero t -> do
      tt <- typeof t
      trowIf (tt /= TyNat) "iszero appl to non-natural"
      pure TyBool