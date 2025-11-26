module LambdaTyped.Typechecker(typechecks) where

import LambdaTyped.Ast

type TyContext = [(String, Type)]

typechecks :: Term -> Type
typechecks = typeof []

typeof :: TyContext -> Term -> Type
typeof ctx term =
  case term of
    TT -> TyBool
    FF -> TyBool
    UU -> TyUnit
    Numb _ -> TyNat
    If cnd t1 t2
      | typeof ctx cnd /= TyBool -> error "if condition must be boolean"
      | typeof ctx t1 /= typeof ctx t2 -> error "if branches must evaluate to same type"
      | otherwise -> typeof ctx t2
    Var str -> 
      case lookup str ctx of
        Just a -> a
        Nothing -> error $ "unknown variable " <> str
    Abs str ty body -> TyFunc ty 
                       $ typeof ((str, ty):ctx) body
    App t1 t2 ->
      case typeof ctx t1 of
        TyFunc TyAny b -> b
        TyFunc a b -> 
          if typeof ctx t2 == a 
            then b
            else error "function paramether of wrong type"
        _ -> error "non-function being applied"
    Let str t1 t2 -> typeof ((str, typeof ctx t1):ctx) t2
    Pair t1 t2 -> typeof ctx t1 :*: typeof ctx t2
    Fst t1 -> 
      case typeof ctx t1 of
        a :*: _ -> a
        _ -> error "non-pair type on fst call"
    Snd t1 ->
      case typeof ctx t1 of
        _ :*: a -> a
        _ -> error "non-pair type on snd call"
    Sum t1 t2
      | typeof ctx t1 /= TyNat || typeof ctx t2 /= TyNat -> error "sum of non-naturals"
      | otherwise -> TyNat
    Mult t1 t2
      | typeof ctx t1 /= TyNat || typeof ctx t2 /= TyNat -> error "product of non-naturals"
      | otherwise -> TyNat
    IsZero t 
      | typeof ctx t /= TyNat -> error "iszero appl to non-natural"
      | otherwise -> TyBool