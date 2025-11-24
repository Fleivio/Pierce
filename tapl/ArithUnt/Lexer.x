{
module ArithUnt.Lexer where
}
%wrapper "posn"


tokens :-

  $white+                ;
  "--".*                 ;

  "true"                 { tokenMk TK_TRUE }
  "false"                { tokenMk TK_FALSE } 

  "if"                   { tokenMk TK_IF }
  "then"                 { tokenMk TK_THEN }
  "else"                 { tokenMk TK_ELSE }
  "0"                    { tokenMk TK_ZERO }
  "succ"                 { tokenMk TK_SUCC }
  "pred"                 { tokenMk TK_PRED }
  "iszero"               { tokenMk TK_IS_ZERO }
  "("                    { tokenMk TK_LPAREN }
  ")"                    { tokenMk TK_RPAREN }

  "\n"                   ;

  .                      { \i c -> error $ "TOKEN ERROR: " ++ show c ++ "\nAt Position:" ++ show i }

{
data TokenId
  = TK_TRUE 
  | TK_FALSE 
  | TK_IF
  | TK_THEN
  | TK_ELSE
  | TK_ZERO
  | TK_SUCC
  | TK_PRED
  | TK_IS_ZERO
  | TK_LPAREN
  | TK_RPAREN
  deriving (Show, Eq)

type Token = (TokenId, (Int, Int))

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a (AlexPn i j k) _ = (a, (i, j))

}