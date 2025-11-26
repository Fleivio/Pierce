module LambdaTyped.Ast(Type(..), Term(..)) where

data Meta = Meta Term String Int Int deriving (Eq, Show)

data Type 
  = TyBool
  | TyUnit
  | TyNat
  | Type :*: Type
  | TyFunc Type Type
  | TyAny 
  deriving(Eq, Show)

data Term
  = TT
  | FF
  | UU
  | Numb Int
  | Abs String Type Term
  | App Term Term
  | Let String Term Term
  | Pair Term Term
  | Fst Term | Snd Term 
  | If Term Term Term
  | Var String
  | Sum Term Term
  | Mult Term Term
  | IsZero Term
  deriving(Eq, Show)
