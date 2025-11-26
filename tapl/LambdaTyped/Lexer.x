{
module LambdaTyped.Lexer(Token, TokenId(..), alexScanTokens) where
}
%wrapper "posn"
$char = [a-z]
$dig = [0-9]
$bar = [\\]

tokens :-

  $white+                ;
  "--".*                 ;

  $bar                 { tokenMk TK_LAM }
  "."                  { tokenMk TK_DOT } 
  "("                  { tokenMk TK_LPAREN } 
  ")"                  { tokenMk TK_RPAREN } 
  ":"                  { tokenMk TK_COLON } 
  "let"                { tokenMk TK_LET}
  "="                  { tokenMk TK_EQ}
  "in"                 { tokenMk TK_IN } 
  "if"                 { tokenMk TK_IF } 
  "then"               { tokenMk TK_THEN } 
  "else"               { tokenMk TK_ELSE } 
  "true"               { tokenMk TK_TRUE } 
  "false"              { tokenMk TK_FALSE } 
  "unit"               { tokenMk TK_UNIT } 
  "fst"                { tokenMk TK_FST } 
  "snd"                { tokenMk TK_SND } 
  "iszero"             { tokenMk TK_IS_ZERO } 
  "B"                  { tokenMk TK_B } 
  "U"                  { tokenMk TK_U } 
  "N"                  { tokenMk TK_N } 
  "A"                  { tokenMk TK_A } 
  "->"                 { tokenMk TK_ARROW } 
  "+"                  { tokenMk TK_SUM } 
  "*"                  { tokenMk TK_PROD } 
  "inl"                { tokenMk TK_INL } 
  "inr"                { tokenMk TK_INR } 
  "{"                  { tokenMk TK_LCBRACK } 
  "}"                  { tokenMk TK_RCBRACK } 
  ","                  { tokenMk TK_COMMA }
  "case"               { tokenMk TK_CASE }
  "of"                 { tokenMk TK_OF }
  "|"                  { tokenMk TK_BAR }

  $char+               { \i c -> tokenMk (TK_STRING c) i c}
  $dig+               { \i c -> tokenMk (TK_NUM (read c)) i c}
  "\n"                 ;

  .                    { \i c -> error $ "TOKEN ERROR: " ++ show c ++ "\nAt Position:" ++ show i }

{
data TokenId
  = TK_LAM
  | TK_DOT
  | TK_STRING String
  | TK_NUM Int
  | TK_IS_ZERO
  | TK_LPAREN | TK_RPAREN
  | TK_LCBRACK | TK_RCBRACK  
  | TK_IF | TK_THEN | TK_ELSE
  | TK_TRUE | TK_FALSE | TK_UNIT
  | TK_COLON | TK_COMMA
  | TK_B | TK_U | TK_N | TK_A | TK_ARROW 
  | TK_LET | TK_EQ | TK_IN
  | TK_SUM | TK_PROD
  | TK_FST | TK_SND
  | TK_INL | TK_INR
  | TK_CASE | TK_OF | TK_BAR  
  deriving (Show, Eq)

type Token = TokenId

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a _ _ = a

} 