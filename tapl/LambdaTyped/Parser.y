{
module LambdaTyped.Parser(parseString) where
import LambdaTyped.Lexer
import LambdaTyped.Ast
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '\\'         { (TK_LAM, $$) }
  '.'          { (TK_DOT, $$) }
  '('          { (TK_LPAREN, $$) }
  ')'          { (TK_RPAREN, $$) }
  'let'        { (TK_LET, $$) }
  'in'         { (TK_IN, $$) }
  '='          { (TK_EQ, $$) }
  'if'         { (TK_IF, $$) }
  'then'       { (TK_THEN, $$) }
  'else'       { (TK_ELSE, $$) }
  'true'       { (TK_TRUE, $$) }
  'false'      { (TK_FALSE, $$) }
  'unit'       { (TK_UNIT, $$) }
  ':'          { (TK_COLON, $$) }
  'B'          { (TK_B, $$) }
  'U'          { (TK_U, $$) }
  'N'          { (TK_N, $$) }
  'A'          { (TK_A, $$) }
  '->'         { (TK_ARROW, $$) }
  '+'          { (TK_SUM, $$) }
  '*'          { (TK_PROD, $$) }
  '{'          { (TK_LCBRACK, $$) }
  '}'          { (TK_RCBRACK, $$) }
  'fst'        { (TK_FST, $$) }
  'snd'        { (TK_SND, $$) }
  ','          { (TK_COMMA, $$) }
  'iszero'     { (TK_IS_ZERO, $$) }
  varid        { (TK_STRING, $$) }
  num          { (TK_NUM, $$) }


%nonassoc 'B' 'false' 'true' 'unit' 
%nonassoc 'if' 'then' 'else' 'fst' 'snd' '=' 'iszero'
%nonassoc varid num '(' ')' '{' '}' 
%left APP
%left '*'
%left '+'
%right '->'
%right 'let' 'in'
%nonassoc '\\' ':' '.'
%%

Program : Term {$1}

Term : '\\' varid ':' Type '.' Term %shift  {Ann $1 \$ Abs (literal $2) $4 $6} 
     | 'let' varid '=' Term 'in' Term       {Ann $1 \$ Let (literal $2) $4 $6}
     | varid                                {Ann $1 \$ Var (literal $1)} 
     | num                                  {Ann $1 \$ Numb (read (literal $1))} 
     | Term Term %prec APP                  {Ann (meta $1) \$ App $1 $2}
     | 'if' Term 'then' Term 'else' Term    {Ann $1 \$ If $2 $4 $6}
     | 'true'                               {Ann $1 \$ TT}
     | 'false'                              {Ann $1 \$ FF}
     | 'unit'                               {Ann $1 \$ UU}
     | 'fst' Term                           {Ann $1 \$ Fst $2}
     | 'snd' Term                           {Ann $1 \$ Snd $2}
     | 'iszero' Term                        {Ann $1 \$ IsZero $2}
     | '{' Term ',' Term '}'                {Ann $1 \$ Pair $2 $4}
     | Term '+' Term                        {Ann $2 \$ Sum $1 $3}
     | Term '*' Term                        {Ann $2 \$ Mult $1 $3}
     | '(' Term ')'                         {$2} 

Type : 'B'            {Ann $1 TyBool}
     | 'U'            {Ann $1 TyUnit}
     | 'N'            {Ann $1 TyNat}
     | 'A'            {Ann $1 TyAny}
     | Type '->' Type {Ann $2 \$ TyFunc $1 $3}
     | Type '*' Type  {Ann $2 \$ TyProd $1 $3}
     | '(' Type ')'   {$2} 
{

parseError (tkn:_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    ]

parseString = parseTokens . alexScanTokens

}
