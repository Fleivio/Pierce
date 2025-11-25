module LambdaTyped.Convert (termToNamed, termToBruijn) where

import qualified LambdaUntyped.Named as N
import qualified LambdaUntyped.Bruijn as B
-- import qualified LambdaUntyped.Convert
import LambdaUntyped.Convert 
import LambdaTyped.Ast

termToNamed :: Term -> N.NTerm
termToNamed term =
  case term of
    Var x -> N.Var x
    App t1 t2 -> termToNamed t1 `N.App` termToNamed t2
    Abs x _ body -> N.Abs x (termToNamed body)
    TT -> N.Abs "@x" (N.Abs "@y" (N.Var "@x"))
    FF -> N.Abs "@x" (N.Abs "@y" (N.Var "@y"))
    If c t1 t2 -> (termToNamed c `N.App` termToNamed t1) `N.App` termToNamed t2
    UU -> N.Var "#u"

termToBruijn :: Term -> B.BTerm
termToBruijn = namedToBruijn . termToNamed
