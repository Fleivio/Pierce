module LambdaUntyped.Convert(namedToBruijn, bruijnToNamed) where


import LambdaUntyped.Named as Named
import LambdaUntyped.Bruijn

import Data.List (subsequences)
import Control.Monad.State

bruijnToNamed :: BTerm -> NTerm
bruijnToNamed bterm = evalState (go bterm) 0
  where
    go :: BTerm -> State Int NTerm -- Keeps the current depth
    go term =
      case term of
        Index n -> gets (Var . genName . subtract n)
        BAbs body -> do
          name <- gets (genName . succ)
          body' <- withState succ (go body)
          modify pred
          pure $ Abs name body'
        BApp t1 t2 -> App 
                      <$> go t1 
                      <*> go t2
    genName n = tail (subsequences ['a'..'z']) !! n

namedToBruijn :: NTerm -> BTerm
namedToBruijn term = evalState (go term) (Named.freeVars term)
    where
      go :: NTerm -> State [String] BTerm
      go tr =
        case tr of
          Var n      -> gets (Index . find n)
          Abs x body -> do
            body' <- withState (x:) (go body)
            modify tail
            pure $ BAbs body'
          App t1 t2  -> BApp 
                        <$> go t1 
                        <*> go t2

      find n (x:xs) | n == x = 0 
                    | otherwise = succ $ find n xs
      find _ [] = error "Hyper-unbounded variable. This should neve happen."