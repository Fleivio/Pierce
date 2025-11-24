module Evaluator(evalM, evalM_, eval) where

import Control.Monad.Identity
import Control.Monad (void)

evalM :: (Monad m, Eq t) => (t -> t) -> (t -> m t) -> t -> m t
evalM method action term = do
  term' <- method <$> action term
  if term' == term
    then pure term
    else evalM method action term'

eval :: Eq t => (t -> t) -> t -> t
eval method term = runIdentity $ evalM method pure term

evalM_ :: (Monad m, Eq t) => (t -> t) -> (t -> m t) -> t -> m ()
evalM_ method action term = void (evalM method action term)
