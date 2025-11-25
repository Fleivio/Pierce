{
module ArithUntyped.Parser(parseTokens) where
import ArithUntyped.Lexer
import ArithUntyped.Ast
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  'true'       { (TK_TRUE, _)}
  'false'      { (TK_FALSE, _)}
  'if'         { (TK_IF, _)}
  'then'       { (TK_THEN, _)}
  'else'       { (TK_ELSE, _)}
  '0'          { (TK_ZERO, _)}
  'succ'       { (TK_SUCC, _) }
  'pred'       { (TK_PRED, _)} 
  'iszero'     { (TK_IS_ZERO, _)}
  '('          { (TK_LPAREN, _)}
  ')'          { (TK_RPAREN, _)}
%%

Program : Term {$1}

Term : 'true'   {TT}
     | 'false'  {FF}
     | 'if' Term 'then' Term 'else' Term {If $2 $4 $6}
     | '0'      {Zero}
     | 'succ' Term {Succ $2}
     | 'pred' Term {Pred $2}
     | 'iszero' Term {Is_Zero $2}
     | '(' Term ')' {$2}


{

parseError ((tkn, pos):_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    , "At line: " ++ show pos
    ]

}
