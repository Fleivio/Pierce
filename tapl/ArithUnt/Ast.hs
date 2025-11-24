{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ArithUnt.Ast(Term(..)) where

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