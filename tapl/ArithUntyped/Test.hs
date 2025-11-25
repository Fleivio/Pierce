module ArithUntyped.Test(test) where

import ArithUntyped.Parser
import ArithUntyped.Lexer
import ArithUntyped.Ast

test :: IO ()
test = do
    let expr = "if iszero (succ 0) then 0 else succ 0"
        tks = alexScanTokens expr
        pars = parseTokens tks
    print pars
    print $ smallEval pars
    print $ bigEval pars