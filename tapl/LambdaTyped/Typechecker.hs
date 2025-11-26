{-# LANGUAGE LambdaCase #-}
module LambdaTyped.Typechecker(typechecks) where

import LambdaTyped.Ast
import Control.Monad.RWS
import Control.Comonad.Identity (Identity)

type TyContext = [(String, Locator Type)]

type Typechecker a = RWS TyContext [String] () a 

trow :: Meta -> String -> Typechecker ()
trow m s = tell [s ++ "\n" ++ show m]

trowIf :: Meta -> Bool -> String -> Typechecker ()
trowIf m True a = trow m a
trowIf m _ _ = pure ()

asAny :: Typechecker a -> Typechecker (Locator Type)
asAny t = t >> pure (atLocal dM TyAny)

typechecks :: Locator Term -> Locator Type
typechecks term = 
  case evalRWS (typeof term) [] () of 
    (a, []) -> a
    (_, errs) -> error $ unlines errs

typeof :: Locator Term -> Typechecker (Locator Type)
typeof term = let meta = getLocal term in
  case extract term of
    TT -> pure (atLocal meta TyBool)
    FF -> pure (atLocal meta TyBool)
    UU -> pure (atLocal meta TyUnit)
    Numb _ -> pure (atLocal meta TyNat)
    If cnd t1 t2 -> do
      condt <- typeof cnd
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf meta (condt /= atLocal meta TyBool) "if condition must be boolean"
      trowIf meta (t1t /= t2t) "if branches must evaluate to same type"
      typeof t2
    Var str -> do
      asks (lookup str) >>= \case
        Nothing -> asAny $ trow meta ("unknown variable " <> str)
        Just a -> return a 
    Abs str ty body -> do
      l <- local ((str,ty):) (typeof body)
      pure $ atLocal meta $ TyFunc ty l 
    App t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      case extract t1t of
        TyFunc a b -> if extract a == TyAny then pure a
                      else trowIf meta (t2t /= a) "function paramether of wrong type"
                      >> pure b
        _ -> asAny $ trow meta "non-function being applied"
    Let str t1 t2 -> do
      t1t <- typeof t1
      local ((str, t1t):) (typeof t2)
    Pair t1 t2 -> 
      atLocal meta (:*:) <$> extract (typeof t1) <*> extract (typeof t2)
    Fst t1 -> do
      typeof t1 >>= \case
        a :*: _ -> pure a
        _ -> asAny $ trow meta "non-pair type on fst call"
    Snd t1 -> do
      typeof t1 >>= \case
        _ :*: a -> pure a
        _ -> asAny $ trow meta "non-pair type on snd call"
    Sum t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf meta (t1t /= TyNat || t2t /= TyNat) "sum of non-naturals"
      pure TyNat
    Mult t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf meta (t1t /= TyNat || t2t /= TyNat) "mult of non-naturals"
      pure TyNat
    IsZero t -> do
      tt <- typeof t
      trowIf meta (tt /= TyNat) "iszero appl to non-natural"
      pure TyBool