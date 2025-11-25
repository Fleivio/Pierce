{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ArithUntyped.Ast(Term(..), bigEval, smallEval', isValue, smallEval) where

data Term 
  = Zero
  | If Term Term Term
  | Succ Term 
  | Pred Term 
  | Is_Zero Term
  | FF
  | TT
  | Stuck
  deriving(Eq, Show)

smallEval :: Term -> Term
smallEval term =
  if isValue term
     then term
     else smallEval (smallEval' term)

smallEval' :: Term -> Term
smallEval' term =
  case term of
    If TT t2 _ -> t2
    If FF _ t3 -> t3
    If y a b -> If (smallEval' y) a b
    Succ a -> Succ (smallEval' a)
    Pred Zero -> Zero
    Pred (Succ v) | isNumeric v -> v
    Is_Zero Zero -> TT
    Is_Zero (Succ v) | isNumeric v -> FF
    Is_Zero a -> Is_Zero (smallEval' a)
    _ -> Stuck

bigEval :: Term -> Term
bigEval term =
  case term of
    If t v1 v2 ->
      case bigEval t of
        TT -> v1
        FF -> v2
        _ -> Stuck
    Succ v ->
      let v' = bigEval v
       in if isNumeric v'
            then Succ v'
            else Stuck
    Pred v ->
      case bigEval v of
        Zero -> Zero
        Succ v' | isNumeric v' -> v'
        _ -> Stuck
    Is_Zero v ->
      case bigEval v of
        Zero -> TT
        k | isNumeric k -> FF
        _ -> Stuck
    v -> v

isValue :: Term -> Bool
isValue term =
  case term of
   TT -> True 
   FF -> True
   a | isNumeric a -> True 
   _ -> False

isNumeric :: Term -> Bool
isNumeric term =
  case term of
    Zero -> True
    Succ nv -> isNumeric nv
    _ -> False
