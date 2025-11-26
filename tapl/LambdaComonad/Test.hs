module LambdaComonad.Test(test) where

import LambdaComonad.Parser
import LambdaComonad.Convert
import LambdaComonad.Typechecker
import Common.Evaluator
import qualified LambdaUntyped.Named as Named 
import qualified LambdaUntyped.Bruijn as Bruijn
import Control.Monad.Writer
import Control.Comonad.Env
import LambdaComonad.Ast

execute :: Locator Term -> IO ()
execute term = do
  let
    named = termToNamed term
    nameless = termToBruijn term
    t1 = execWriter $ evalM Named.normalOrder (\a -> writer (a, [show a])) named
    t2 = execWriter $ evalM Bruijn.normalOrder (\a -> writer (a, [show a])) nameless 
  

  putStrLn $ unlines 
    ["Named: "
    , unlines t1
    , "Nameless: "
    , unlines t2
    ]
  

test :: IO ()
test = do 
  concrete <- readFile "tapl/LambdaComonad/input.lang"
  let parsed = parseString concrete

  case typechecks parsed of 
    Left err -> putStrLn $ "Type error: \n" ++ err
    Right t ->
      do  putStrLn $ "Typechecking successful: " ++ show (extract t) 
          execute parsed 
