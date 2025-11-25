{
module LambdaUntyped.Lexer(Token, TokenId(..), alexScanTokens) where
}
%wrapper "posn"
$char = [a-z]
$bar = [\\]

tokens :-

  $white+                ;
  "--".*                 ;

  $bar                 { tokenMk TK_LAM }
  "."                  { tokenMk TK_DOT } 
  "("                  { tokenMk TK_LPAREN } 
  ")"                  { tokenMk TK_RPAREN } 
  $char+               { \i c -> tokenMk (TK_STRING c) i c}
  "\n"                 ;

  .                    { \i c -> error $ "TOKEN ERROR: " ++ show c ++ "\nAt Position:" ++ show i }

{
data TokenId
  = TK_LAM
  | TK_DOT
  | TK_STRING String
  | TK_LPAREN
  | TK_RPAREN
  deriving (Show, Eq)

type Token = TokenId

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a _ _ = a

}