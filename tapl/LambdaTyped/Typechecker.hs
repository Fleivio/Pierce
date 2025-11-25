module LambdaTyped.Typechecker(typechecks) where

import LambdaTyped.Ast

type TyContext = [(String, Type)]
type DefContext = [(String, Term)]

getType :: TyContext -> String -> Maybe Type
getType [] _ = Nothing
getType ((x, ty):_) s | s == x = Just ty 
getType (_:xs) s = getType xs s 

typechecks :: Term -> Type
typechecks = typeof []

typeof :: TyContext -> Term -> Type
typeof ctx term =
  case term of
    TT -> TyBool
    FF -> TyBool
    UU -> TyUnit
    If cnd t1 t2
      | typeof ctx cnd /= TyBool -> error "if condition must be boolean"
      | typeof ctx t1 /= typeof ctx t2 -> error "if branches must evaluate to same type"
      | otherwise -> typeof ctx t2
    Var str -> 
      case getType ctx str of
        Just a -> a
        Nothing -> error $ "unknown variable " <> str
    Abs str ty body -> TyFunc ty 
                       $ typeof ((str, ty):ctx) body
    App t1 t2 ->
      case typeof ctx t1 of
        TyFunc a b -> 
          if typeof ctx t2 == a 
            then b
            else error "function paramether of wrong type"
        _ -> error "non-function being applied"