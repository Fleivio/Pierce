module LambdaUntyped.Convert(namedToBruijn, bruijnToNamed) where


import LambdaUntyped.Named as Named
import LambdaUntyped.Bruijn

import Data.List (subsequences)
import Control.Monad.Reader

bruijnToNamed :: BTerm -> NTerm
bruijnToNamed bterm = runReader (go bterm) 0
  where
    go :: BTerm -> Reader Int NTerm -- Keeps the current depth
    go term =
      case term of
        Index n -> asks (Var . genName . subtract n)
        BAbs body -> do
          name <- asks (genName . succ)
          body' <- local succ (go body)
          pure $ Abs name body'
        BApp t1 t2 -> App 
                      <$> go t1 
                      <*> go t2
    genName n = tail (subsequences ['a'..'z']) !! n

namedToBruijn :: NTerm -> BTerm
namedToBruijn term = runReader (go term) (Named.freeVars term)
    where
      go :: NTerm -> Reader [String] BTerm
      go tr =
        case tr of
          Var n      -> asks (Index . find n)
          Abs x body -> do
            body' <- local (x:) (go body)
            pure $ BAbs body'
          App t1 t2  -> BApp 
                        <$> go t1 
                        <*> go t2

      find n (x:xs) | n == x = 0 
                    | otherwise = succ $ find n xs
      find _ [] = error "Hyper-unbounded variable. This should neve happen."