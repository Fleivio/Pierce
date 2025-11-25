module LambdaTyped.Ast(Type(..), Term(..)) where

data Type 
  = TyBool
  | TyUnit
  | TyFunc Type Type
  deriving(Eq, Show)

data Term
  = TT
  | FF
  | UU
  | If Term Term Term
  | Var String
  | Abs String Type Term
  | App Term Term
  deriving(Eq, Show)
