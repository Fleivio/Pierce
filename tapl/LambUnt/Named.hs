module LambUnt.Named(Term(..), normalOrder, callByVal, callByName, freeVars, boundVars) where
import Data.List

data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving(Eq)

instance Show Term where
  show term =
    case term of
      Var n -> n
      Abs x body -> "(\\" <> x <> ". " <> show body <> ")"
      App t1 t2 -> show t1 <> " " <> show t2

freeVars :: Term -> [String]
freeVars term =
  case term of
    Var n -> pure n
    App t1 t2 -> freeVars t1 ++ freeVars t2
    Abs x body -> x `delete` freeVars body

boundVars :: Term -> [String]
boundVars term =
  case term of
    Var _ -> mempty
    App t1 t2 -> boundVars t1 <> boundVars t2
    Abs x body -> x : boundVars body

alphaConv :: Term -> String -> String -> Term
alphaConv term oldName newName =
  case term of
    Var n | n == oldName -> Var newName
    Abs x body -> Abs x (alphaConv body oldName newName)
    App t1 t2 -> alphaConv t1 oldName newName `App` alphaConv t2 oldName newName
    n -> n

genNewName :: Term -> String -> String
genNewName term name = head $ filter (`notElem` boundVars term) names
  where names = [ replicate factor '~' ++ name | factor <- [1..]]

betaRed :: String -> Term -> Term -> Term
betaRed name param expr =
  case expr of
    Var n | n == name -> param
    App t1 t2 -> betaRed name param t1 `App` betaRed name param t2
    Abs x body | x /= name ->
      if x `notElem` freeVars param
        then Abs x $ betaRed name param body
        else
          let nName = genNewName body x
          in Abs nName $ betaRed name param (alphaConv body x nName)
    n -> n

_etaRed :: Term -> Term
_etaRed (Abs x (App f t2)) | Var x == t2 = f
_etaRed a = a

type Step = Term -> Term

normalOrder :: Step
normalOrder  term =
  case term of
    Abs x body -> Abs x (normalOrder body)
    App (Abs x body) targ -> betaRed x targ body
    App t1 t2 -> normalOrder t1 `App` normalOrder t2
    n -> n

isValue :: Term -> Bool
isValue term =
  case term of
    App {} -> False
    _ -> True

callByVal :: Step
callByVal term =
  case term of
    App (Abs x body) targ
      | isValue targ -> betaRed x targ body
      | otherwise -> betaRed x targ (callByVal targ)
    App t1 t2 -> App (callByVal t1) t2
    a -> a

callByName :: Step
callByName term =
  case term of
    App (Abs x body) targ -> betaRed x targ body
    App t1 t2 -> App (callByName t1) t2
    a -> a
