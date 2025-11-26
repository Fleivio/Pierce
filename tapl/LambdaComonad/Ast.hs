module LambdaComonad.Ast(Type(..), Term(..), (<@>), Locator, Comonad(..), Meta(..), getLocal, dM, atLocal) where

import Control.Comonad.Env

data Meta 
  = Meta {
    literal :: String
  , line :: Int
  , row :: Int
  } deriving (Eq, Show)

dM :: Meta
dM = Meta { literal = "", line = 0, row = 0 }

atLocal :: Meta -> a (Env Meta) -> Locator a
atLocal = env

type Locator a = (Env Meta (a (Env Meta)))

getLocal :: Locator a -> Meta
getLocal = ask

data Comonad m => Type m 
  = TyBool
  | TyUnit
  | TyNat
  | TyProd (m (Type m)) (m (Type m))
  | TyFunc (m (Type m)) (m (Type m))
  | TyAny 

data Comonad m => Term m
  = TT
  | FF
  | UU
  | Numb Int
  | Abs String (m (Type m)) (m (Term m))
  | App (m (Term m)) (m (Term m))
  | Let String (m (Term m)) (m (Term m))
  | Pair (m (Term m)) (m (Term m))
  | Fst (m (Term m)) 
  | Snd (m (Term m)) 
  | If (m (Term m)) (m (Term m)) (m (Term m))
  | Var String
  | Sum (m (Term m)) (m (Term m))
  | Mult (m (Term m)) (m (Term m))
  | IsZero (m (Term m))

instance Comonad m => Show (Type m) where
  show TyBool = "Bool"
  show TyUnit = "Unit"
  show TyNat = "Nat"
  show TyAny = "Any"
  show (TyProd a b) =
    let x = extract a
        y = extract b
    in "(" ++ show x ++ " * " ++ show y ++ ")"
  show (TyFunc a b) =
    let x = extract a
        y = extract b
    in "(" ++ show x ++ " -> " ++ show y ++ ")"

instance Comonad m => Eq (Type m) where
  TyBool == TyBool = True
  TyUnit == TyUnit = True
  TyNat == TyNat = True
  TyAny == TyAny = True
  (TyProd a1 b1) == (TyProd a2 b2) =
    extract a1 == extract a2 && extract b1 == extract b2
  (TyFunc a1 b1) == (TyFunc a2 b2) =
    extract a1 == extract a2 && extract b1 == extract b2
  _ == _ = False
  
instance Comonad m => Show (Term m) where
  show TT = "true"
  show FF = "false"
  show UU = "unit"
  show (Numb n) = show n
  show (Abs str ty body) =
    let b = extract body 
    in "(λ" ++ str ++ ":" ++ show (extract ty) ++ ". " ++ show b ++ ")"
  show (App t1 t2) =
    let a = extract t1
        b = extract t2
    in "(" ++ show a ++ " " ++ show b ++ ")"
  show (Let str t1 t2) =
    let a = extract t1
        b = extract t2
    in "(let " ++ str ++ " = " ++ show a ++
       " in " ++ show b ++ ")"
  show (Pair t1 t2) =
    let a = extract t1
        b = extract t2
    in "(" ++ show a ++ ", " ++ show b ++ ")"
  show (Fst t1) =
    let a = extract t1
    in "fst " ++ show a
  show (Snd t1) =
    let a = extract t1
    in "snd " ++ show a
  show (If cnd t1 t2) =
    let c = extract cnd
        a = extract t1
        b = extract t2
    in "if " ++ show c ++ " then " ++ show a ++
       " else " ++ show b
  show (Var str) = str
  show (Sum t1 t2) =
    let a = extract t1
        b = extract t2
    in "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Mult t1 t2) =
    let a = extract t1
        b = extract t2
    in "(" ++ show a ++ " * " ++ show b ++ ")"
  show (IsZero t) =
    let a = extract t
    in "iszero " ++ show a

  