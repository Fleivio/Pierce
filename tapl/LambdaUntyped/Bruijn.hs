module LambdaUntyped.Bruijn(BTerm(..), normalOrder, callByVal, callByName, freeVars) where
import Data.List (delete)


data BTerm
  = Index Int
  | BApp BTerm BTerm
  | BAbs BTerm
  deriving (Eq)

instance Show BTerm where
  show term =
    case term of
      Index ix    -> show ix
      BAbs body -> "(\\." <> show body <> ")"
      BApp t1 t2  -> show t1 <> " " <> show t2 

isValue :: BTerm -> Bool
isValue term =
  case term of
    BApp {} -> False
    _ -> True 

shift :: Int -> Int -> BTerm -> BTerm
shift depth a term =
  case term of
    Index i | i < depth -> Index i
            | otherwise  -> Index $ i + a --free
    BAbs body -> BAbs (shift (depth + 1) a body)
    BApp t1 t2 -> shift depth a t1 `BApp` shift depth a t2


subst :: Int -> BTerm -> BTerm -> BTerm
subst j term body =
  case body of
    Index i | i == j -> term
            | otherwise -> body
    BAbs b     -> BAbs $ subst (succ j) (shift 0 1 term) b
    BApp t1 t2 -> subst j term t1 `BApp` subst j term t2 

betaRed :: BTerm -> BTerm -> BTerm 
betaRed param expr = 
  let 
    lifted  = shift 0 1 param -- cuz itll enter the BAbs
    reduced = subst 0 lifted expr -- subst from depth 0
  in shift 0 (-1) reduced -- bye bye outermost BAbs

freeVars :: BTerm -> [Int]
freeVars term =
  case term of
    Index k -> [k]
    BAbs body -> delete 0 (freeVars body)
    BApp t1 t2 -> freeVars t1 <> freeVars t2

type BStep = BTerm -> BTerm

normalOrder :: BStep
normalOrder  term =
  case term of
    BAbs body -> BAbs (normalOrder body)
    BApp (BAbs body) targ -> betaRed targ body
    BApp t1 t2 -> normalOrder t1 `BApp` normalOrder t2
    n -> n

callByVal :: BStep
callByVal term =
  case term of
    BApp (BAbs body) targ
      | isValue targ -> betaRed targ body
      | otherwise -> betaRed targ (callByVal targ)
    BApp t1 t2 -> BApp (callByVal t1) t2
    a -> a

callByName :: BStep
callByName term =
  case term of
    BApp (BAbs body) targ -> betaRed targ body
    BApp t1 t2 -> BApp (callByName t1) t2
    a -> a

