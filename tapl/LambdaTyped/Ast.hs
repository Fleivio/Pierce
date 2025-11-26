module LambdaTyped.Ast(Type(..), Term(..), Meta(..), getMetaT) where

data Meta = Meta {
    literal :: String
  , lin :: Int
  , col :: Int
  }

instance Show Meta where
  show (Meta lit l c) = "At line " <> show l <> ", column " <> show c <> ": " <> show lit

data Type 
  = TyBool Meta
  | TyUnit Meta
  | TyNat Meta 
  | TyProd Meta Type Type
  | TyFunc Meta Type Type
  | TyAny Meta
  deriving(Show)

instance Eq Type where
  (TyBool _) == (TyBool _) = True
  (TyUnit _) == (TyUnit _) = True
  (TyNat _) == (TyNat _) = True
  (TyProd _ a1 b1) == (TyProd _ a2 b2) = a1 == a2 && b1 == b2
  (TyFunc _ a1 b1) == (TyFunc _ a2 b2) = a1 == a2 && b1 == b2
  (TyAny _) == (TyAny _) = True
  _ == _ = False

data Term
  = TT Meta
  | FF Meta
  | UU Meta
  | Numb Meta Int
  | Abs Meta String Type Term
  | App Meta Term Term
  | Let Meta String Term Term
  | Pair Meta Term Term
  | Fst Meta Term 
  | Snd Meta Term 
  | If Meta Term Term Term
  | Var Meta String
  | Sum Meta Term Term
  | Mult Meta Term Term
  | IsZero Meta Term

instance Show Term where
  show (TT _) = "true"
  show (FF _) = "false"
  show (UU _) = "unit"
  show (Numb _ n) = show n
  show (Abs _ s t b) = "(\\" <> s <> ":" <> show t <> ". " <> show b <> ")"
  show (App _ f a) = "(" <> show f <> " " <> show a <> ")"
  show (Let _ s t b) = "(let " <> s <> " = " <> show t <> " in " <> show b <> ")"
  show (Pair _ l r) = "<" <> show l <> ", " <> show r <> ">"
  show (Fst _ p) = "(fst " <> show p <> ")"
  show (Snd _ p) = "(snd " <> show p <> ")"
  show (If _ c t e) = "(if " <> show c <> " then " <> show t <> " else " <> show e <> ")"
  show (Var _ s) = s
  show (Sum _ m n) = "(" <> show m <> " + " <> show n <> ")"
  show (Mult _ m n) = "(" <> show m <> " * " <> show n <> ")"
  show (IsZero _ t) = "(iszero " <> show t <> ")"

instance Eq Term where
  (TT _) == (TT _) = True
  (FF _) == (FF _) = True
  (UU _) == (UU _) = True
  (Numb _ n1) == (Numb _ n2) = n1 == n2
  (Abs _ s1 t1 b1) == (Abs _ s2 t2 b2) = s1 == s2 && t1 == t2 && b1 == b2
  (App _ f1 a1) == (App _ f2 a2) = f1 == f2 && a1 == a2
  (Let _ s1 t1 b1) == (Let _ s2 t2 b2) = s1 == s2 && t1 == t2 && b1 == b2
  (Pair _ l1 r1) == (Pair _ l2 r2) = l1 == l2 && r1 == r2
  (Fst _ p1) == (Fst _ p2) = p1 == p2
  (Snd _ p1) == (Snd _ p2) = p1 == p2
  (If _ c1 t1 e1) == (If _ c2 t2 e2) = c1 == c2 && t1 == t2 && e1 == e2
  (Var _ s1) == (Var _ s2) = s1 == s2
  (Sum _ m1 n1) == (Sum _ m2 n2) = m1 == m2 && n1 == n2
  (Mult _ m1 n1) == (Mult _ m2 n2) = m1 == m2 && n1 == n2
  (IsZero _ t1) == (IsZero _ t2) = t1 == t2
  _ == _ = False

getMetaT :: Term -> Meta
getMetaT term =
  case term of
    TT m -> m
    FF m -> m
    UU m -> m
    Numb m _ -> m
    Abs m _ _ _ -> m
    App m _ _ -> m
    Let m _ _ _ -> m
    Pair m _ _ -> m
    Fst m _ -> m
    Snd m _ -> m
    If m _ _ _ -> m
    Var m _ -> m
    Sum m _ _ -> m
    Mult m _ _ -> m
    IsZero m _ -> m