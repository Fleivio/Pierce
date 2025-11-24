{-# LANGUAGE TypeApplications #-}
module LambUnt.Test(test) where

import LambUnt.Parser
import LambUnt.Converter
import LambUnt.Evaluator
import LambUnt.Named as Named
import LambUnt.Bruijn as Bruijn

import Control.Monad.Writer
-- import Control.Monad.State

test :: IO ()
test = do
  let concrete = "(\\y x. y x) (\\z. z x) (\\a. a)"
      pars = parseString concrete
      brj = toBruijn pars
      reconv = toNamed brj

  let t1 = execWriter $ evalM Named.normalOrder (\a -> writer (a, [show a])) pars
      t2 = execWriter $ evalM Bruijn.normalOrder (\a -> writer (a, [show a])) brj
      t3 = execWriter $ evalM Named.normalOrder (\a -> writer (a, [show a])) reconv

  -- number of steps using state monad
  -- let t4 = execState (evalM Named.normalOrder (\a -> modify succ >> pure a) reconv) 0

  putStrLn $ unlines 
    ["Original entry: " <> show pars
    , unlines t1
    , "Bruijn conversion: " <> show brj
    , unlines t2
    , "Named re-conversion: " <> show reconv
    , unlines t3
    ]


  
