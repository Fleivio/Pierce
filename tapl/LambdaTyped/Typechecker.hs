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
typeof term =
  case term of
    TT m -> pure $ TyBool m
    FF m -> pure $ TyBool m
    UU m -> pure $ TyUnit m
    Numb m _ -> pure $ TyNat m
    If m cnd t1 t2 -> do
      condt <- typeof cnd
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (condt /= TyBool m) m "if condition must be boolean"
      trowIf (t1t /= t2t) m "if branches must evaluate to same type"
      typeof t2
    Var m str -> do
      asks (lookup str) >>= \case
        Nothing -> trow m ("unknown variable " <> str) >> pure (TyAny m)
        Just a -> return a 
    Abs m str ty body -> TyFunc m ty <$> local ((str,ty):) (typeof body)
    App m t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      case t1t of
        TyFunc _ (TyAny _) b -> pure b
        TyFunc _ a b -> trowIf (t2t /= a) m "function paramether of wrong type"
                      >> pure b
        _ -> trow m "non-function being applied" >> pure (TyAny m)
    Let _ str t1 t2 -> do
      t1t <- typeof t1
      local ((str, t1t):) (typeof t2)
    Pair m t1 t2 -> TyProd m <$> typeof t1 <*> typeof t2
    Fst m t1 -> do
      typeof t1 >>= \case
        TyProd _ a _ -> pure a
        _ -> trow m "non-pair type on fst call" >> pure (TyAny m)
    Snd m t1 -> do
      typeof t1 >>= \case
        TyProd _ _ a -> pure a
        _ -> trow m "non-pair type on snd call" >> pure (TyAny m)
    Sum m t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (t1t /= TyNat m || t2t /= TyNat m) m "sum of non-naturals"
      pure $ TyNat m
    Mult m t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf (t1t /= TyNat m || t2t /= TyNat m) m "mult of non-naturals"
      pure $ TyNat m
    IsZero m t -> do
      tt <- typeof t
      trowIf (tt /= TyNat m) m "iszero appl to non-natural"
      pure $ TyBool m