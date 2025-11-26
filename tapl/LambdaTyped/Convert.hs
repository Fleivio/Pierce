module LambdaTyped.Convert (termToNamed, termToBruijn) where

import qualified LambdaUntyped.Named as N
import qualified LambdaUntyped.Bruijn as B
import LambdaUntyped.Convert
import LambdaTyped.Ast

toChurch :: Int -> N.NTerm
toChurch k = N.Abs "@s" $ N.Abs "@z" inner
  where inner = foldl (flip N.App) (N.Var "@z") (replicate k (N.Var "@s"))

termToNamed :: Term -> N.NTerm
termToNamed term =
  case term of
    Var _ x -> N.Var x
    App _ t1 t2 -> termToNamed t1 `N.App` termToNamed t2
    Abs _ x _ body -> N.Abs x (termToNamed body)
    TT _ -> tt
    FF _ -> ff
    If _ c t1 t2 -> (termToNamed c `N.App` termToNamed t1) `N.App` termToNamed t2
    UU _ -> N.Var "#u"
    Let _ x t1 t2 -> N.Abs x (termToNamed t2) `N.App` termToNamed t1
    Fst _ x -> termToNamed x `N.App` tt
    Snd _ x -> termToNamed x `N.App` ff
    Pair _ t1 t2 -> N.Abs "@selector" (N.Var "@selector" `N.App` termToNamed t1 `N.App` termToNamed t2)
    Numb _ k -> toChurch k
    Sum _ m n  -> N.Abs "@s" $ N.Abs "@z" 
                  (termToNamed m `N.App` N.Var "@s" `N.App` 
                    (termToNamed n `N.App` N.Var "@s" `N.App` N.Var "@z"))
    Mult _ m n -> N.Abs "@s" $ termToNamed m `N.App` (termToNamed n `N.App` N.Var "@s")
    IsZero _ m -> termToNamed m `N.App` N.Abs "_" ff `N.App` tt
  where
    tt = N.Abs "@x" (N.Abs "@y" (N.Var "@x"))
    ff = N.Abs "@x" (N.Abs "@y" (N.Var "@y"))


termToBruijn :: Term -> B.BTerm
termToBruijn = namedToBruijn . termToNamed
