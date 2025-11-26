module LambdaTyped.Test(test) where

import LambdaTyped.Parser
import LambdaTyped.Convert
import LambdaTyped.Typechecker
import Common.Evaluator
import qualified LambdaUntyped.Named as Named 
import qualified LambdaUntyped.Bruijn as Bruijn
import Control.Monad.Writer

test :: IO ()
test = do 
  concrete <- readFile "tapl/LambdaTyped/input.lang"
  let parsed = parseString concrete
      typ = typechecks parsed
      named = termToNamed parsed
      nameless = termToBruijn parsed
  -- print parsed
  print typ
  let
    t1 = execWriter $ evalM Named.normalOrder (\a -> writer (a, [show a])) named
    t2 = execWriter $ evalM Bruijn.normalOrder (\a -> writer (a, [show a])) nameless 
  

  putStrLn $ unlines 
    ["Named: "
    , unlines t1
    , "Nameless: " 
    , unlines t2
    ]
  


