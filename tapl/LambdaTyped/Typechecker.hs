{-# LANGUAGE LambdaCase #-}
module LambdaTyped.Typechecker(typechecks) where

import LambdaTyped.Ast
import Control.Monad.RWS

type TyContext = [(String, Type)]

type Typechecker a = RWS TyContext [String] () a 

trow :: Meta -> String -> Typechecker ()
trow m s = tell ["\n" <> s <> "\n" <> show m <> "\n"]

trowIf ::  Bool -> Meta -> String -> Typechecker ()
trowIf True m a = trow m a
trowIf _ _ _ = pure ()

typechecks :: Term -> Type
typechecks term = 
  case evalRWS (typeof term) [] () of 
    (a, []) -> a
    (_, errs) -> error $ unlines errs

typeof :: Term -> Typechecker Type
typeof (Ann m term) =
  case term of
    TT -> pure $ Ann m TyBool
    FF -> pure $ Ann m TyBool 
    UU -> pure $ Ann m  TyUnit
    Numb _ -> pure $ Ann m TyNat
    If cnd t1 t2 -> do
      Ann _ condt <- typeof cnd
      Ann _ t1t <- typeof t1
      Ann _ t2t <- typeof t2
      trowIf (condt /= TyBool) m "if condition must be boolean"
      trowIf (t1t /= t2t) m "if branches must evaluate to same type"
      typeof t2
    Var str -> do
      k <- asks (lookup str)
      case k of
        Nothing -> do 
          trow m ("unknown variable " <> str)
          pure $ Ann m TyAny
        Just a -> return a 
    Abs str ty body -> do
      bodyT <- local ((str,ty):) (typeof body)
      pure $ Ann m (TyFunc ty bodyT)
    App t1 t2 -> do
      Ann _ t1t <- typeof t1
      Ann _ t2t <- typeof t2
      case t1t of
        TyFunc (Ann _ TyAny) b -> pure b
        TyFunc (Ann _ a) b -> trowIf (t2t /= a) m "function paramether of wrong type"
                              >> pure b
        _ -> trow m "non-function being applied" >> pure (Ann m TyAny)
    Let str t1 t2 -> do
      t1t <- typeof t1
      local ((str, t1t):) (typeof t2)
    Pair t1 t2 -> Ann m <$> (TyProd <$> typeof t1 <*> typeof t2)
    Fst t1 -> do
      Ann _ t1t <- typeof t1
      case t1t of
        TyProd a _ -> pure a
        _ -> trow m "non-pair type on fst call" >> pure (Ann m TyAny)
    Snd t1 -> do
      Ann _ t1t <- typeof t1
      case t1t of
        TyProd _ a -> pure a
        _ -> trow m "non-pair type on snd call" >> pure (Ann m TyAny)
    Sum t1 t2 -> do
      Ann _ t1t <- typeof t1
      Ann _ t2t <- typeof t2
      trowIf (t1t /= TyNat|| t2t /= TyNat) m "sum of non-naturals"
      pure $ Ann m TyNat
    Mult t1 t2 -> do
      Ann _ t1t <- typeof t1
      Ann _ t2t <- typeof t2
      trowIf (t1t /= TyNat || t2t /= TyNat) m "mult of non-naturals"
      pure $ Ann m TyNat
    IsZero t -> do
      Ann _ tt <- typeof t
      trowIf (tt /= TyNat) m "iszero appl to non-natural"
      pure $ Ann m TyBool