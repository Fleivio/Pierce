module LambdaTyped.Convert (termToNamed, termToBruijn) where

import qualified LambdaUntyped.Named as N
import qualified LambdaUntyped.Bruijn as B
import LambdaUntyped.Convert
import LambdaTyped.Ast

toChurch :: Int -> N.NTerm
toChurch k = N.Abs "@s" $ N.Abs "@z" inner
  where inner = foldl (flip N.App) (N.Var "@z") (replicate k (N.Var "@s"))

termToNamed :: Comonad m => m (Term m) -> N.NTerm
termToNamed term =
  case extract term of
    Var x -> N.Var x
    App t1 t2 -> termToNamed t1 `N.App` termToNamed t2
    Abs x _ body -> N.Abs x (termToNamed body)
    TT -> tt
    FF -> ff
    If c t1 t2 -> (termToNamed c `N.App` termToNamed t1) 
                  `N.App` termToNamed t2
    UU -> N.Var "#u"
    Let x t1 t2 -> N.Abs x (termToNamed t2) `N.App` termToNamed t1
    Fst x -> termToNamed x `N.App` tt
    Snd x -> termToNamed x `N.App` ff
    Pair t1 t2 -> N.Abs "@selector"
                  (N.Var "@selector" 
                  `N.App` termToNamed t1 
                  `N.App` termToNamed t2)
    Numb k -> toChurch k
    Sum m n  -> N.Abs "@s" $ N.Abs "@z" 
                  (termToNamed m `N.App` N.Var "@s" `N.App` 
                    (termToNamed n `N.App` N.Var "@s" `N.App` N.Var "@z"))
    Mult m n -> N.Abs "@s" $ termToNamed m `N.App` (termToNamed n `N.App` N.Var "@s")
    IsZero m -> termToNamed m `N.App` N.Abs "_" ff `N.App` tt
  
  where 
    tt = N.Abs "@x" (N.Abs "@y" (N.Var "@x"))
    ff = N.Abs "@x" (N.Abs "@y" (N.Var "@y"))


termToBruijn :: Comonad m => m (Term m) -> B.BTerm
termToBruijn = namedToBruijn . termToNamed
