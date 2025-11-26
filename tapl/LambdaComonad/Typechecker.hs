{-# LANGUAGE LambdaCase #-}
module LambdaComonad.Typechecker(typechecks) where

import LambdaComonad.Ast
import Control.Monad.RWS
-- import Control.Comonad.Identity (Identity)

type TyContext = [(String, Locator Type)]

type Typechecker a = RWS TyContext [String] () a 

trow :: Meta -> String -> Typechecker ()
trow m s = tell [s ++ "\n" ++ show m]

trowIf :: Meta -> Bool -> String -> Typechecker ()
trowIf m True a = trow m a
trowIf _ _ _ = pure ()

asAny :: Typechecker a -> Typechecker (Locator Type)
asAny t = t >> pure (atLocal dM TyAny)

typechecks :: Locator Term -> Either String (Locator Type)
typechecks term = 
  case evalRWS (typeof term) [] () of 
    (a, []) -> Right a
    (_, errs) -> Left $ unlines errs

typeof :: Locator Term -> Typechecker (Locator Type)
typeof term = let m = getLocal term in
  case extract term of
    TT -> pure (atLocal m TyBool)
    FF -> pure (atLocal m TyBool)
    UU -> pure (atLocal m TyUnit)
    Numb _ -> pure (atLocal m TyNat)
    If cnd t1 t2 -> do
      condt <- typeof cnd
      t1t <- typeof t1
      t2t <- typeof t2
      trowIf m (extract condt /= TyBool) "if condition must be boolean"
      trowIf m (extract t1t /= extract t2t) "if branches must evaluate to same type"
      typeof t2
    Var str -> do
      asks (lookup str) >>= \case
        Nothing -> asAny $ trow m ("unknown variable " <> str)
        Just a -> return a 
    Abs str ty body -> do
      l <- local ((str,ty):) (typeof body)
      pure $ atLocal m $ TyFunc ty l 
    App t1 t2 -> do
      t1t <- typeof t1
      t2t <- typeof t2
      case extract t1t of
        TyFunc a b -> if extract a == TyAny then pure a
                      else trowIf m (extract t2t /= extract a) "function paramether of wrong type"
                      >> pure b
        _ -> asAny $ trow m "non-function being applied"
    Let str t1 t2 -> do
      t1t <- typeof t1
      local ((str, t1t):) (typeof t2)
    Pair t1 t2 -> do
      a <- typeof t1
      b <- typeof t2
      pure $ atLocal m $ a `TyProd` b
    Fst t1 -> do
      t1t <- typeof t1 
      case extract t1t of
        a `TyProd` _ -> pure a
        _ -> asAny $ trow m "non-pair type on fst call"
    Snd t1 -> do
      t1t <- typeof t1
      case extract t1t of
        _ `TyProd` a -> pure a
        _ -> asAny $ trow m "non-pair type on snd call"
    Sum t1 t2 -> do
      t1t <- extract <$> typeof t1
      t2t <- extract <$> typeof t2
      trowIf m (t1t /= TyNat || t2t /= TyNat) "sum of non-naturals"
      pure $ atLocal m TyNat
    Mult t1 t2 -> do
      t1t <- extract <$> typeof t1
      t2t <- extract <$> typeof t2
      trowIf m (t1t /= TyNat || t2t /= TyNat) "mult of non-naturals"
      pure $ atLocal m TyNat
    IsZero t -> do
      tt <- extract <$> typeof t
      trowIf m (tt /= TyNat) "iszero appl to non-natural"
      pure $ atLocal m TyBool