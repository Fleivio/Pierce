{
module LambdaTyped.Lexer(Token, TokenId(..), alexScanTokens) where
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
  ":"                  { tokenMk TK_COLON } 
  "def"                { tokenMk TK_DEF}
  "="                  { tokenMk TK_EQ}
  ";"                  { tokenMk TK_DELIM } 
  "if"                 { tokenMk TK_IF } 
  "then"               { tokenMk TK_THEN } 
  "else"               { tokenMk TK_ELSE } 
  "true"               { tokenMk TK_TRUE } 
  "false"              { tokenMk TK_FALSE } 
  "unit"               { tokenMk TK_UNIT } 
  "B"                  { tokenMk TK_B } 
  "U"                  { tokenMk TK_U } 
  "->"                 { tokenMk TK_ARROW } 
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
  | TK_IF
  | TK_THEN
  | TK_ELSE
  | TK_TRUE
  | TK_FALSE
  | TK_UNIT
  | TK_COLON
  | TK_B
  | TK_U 
  | TK_ARROW 
  | TK_DEF
  | TK_EQ
  | TK_DELIM
  deriving (Show, Eq)

type Token = TokenId

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a _ _ = a

} 