{
module LambdaTyped.Parser(parseString) where
import LambdaTyped.Lexer
import LambdaTyped.Ast
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '\\'         { (TK_LAM, _) }
  '.'          { (TK_DOT, _) }
  '('          { (TK_LPAREN, _) }
  ')'          { (TK_RPAREN, _) }
  'let'        { (TK_LET, _) }
  'in'         { (TK_IN, _) }
  '='          { (TK_EQ, _) }
  'if'         { (TK_IF, _) }
  'then'       { (TK_THEN, _) }
  'else'       { (TK_ELSE, _) }
  'true'       { (TK_TRUE, _) }
  'false'      { (TK_FALSE, _) }
  'unit'       { (TK_UNIT, _) }
  ':'          { (TK_COLON, _) }
  'B'          { (TK_B, _) }
  'U'          { (TK_U, _) }
  'N'          { (TK_N, _) }
  'A'          { (TK_A, _) }
  '->'         { (TK_ARROW, _) }
  '+'          { (TK_SUM, _) }
  '*'          { (TK_PROD, _) }
  '{'          { (TK_LCBRACK, _) }
  '}'          { (TK_RCBRACK, _) }
  'fst'        { (TK_FST, _) }
  'snd'        { (TK_SND, _) }
  ','          { (TK_COMMA, _) }
  'iszero'     { (TK_IS_ZERO, _) }
  varid        { (TK_STRING $$, _) }
  num          { (TK_NUM $$, _) }


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

Program : Term {atLocal dM $1}

Term : '\\' varid ':' Type '.' Term %shift {Abs <\$> $2 <@> $4 <@> $6} 
     | 'let' varid '=' Term 'in' Term      {Let <\$> $2 <@> $4 <@> $6}
     | varid                               {Var <\$> $1} 
     | num                                 {Numb <\$> $1} 
     | Term Term %prec APP                 {App <\$> $1 <@> $2}
     | 'if' Term 'then' Term 'else' Term   {If <\$> $2 <@> $4 <@> $6}
     | 'true'                              {TT}
     | 'false'                             {FF}
     | 'unit'                              {UU}
     | 'fst' Term                          {Fst <\$> $2}
     | 'snd' Term                          {Snd <\$> $2}
     | 'iszero' Term                       {IsZero <\$> $2}
     | '{' Term ',' Term '}'               {Pair <\$> $2 <@> $4}
     | Term '+' Term                       {Sum <\$> $1 <@> $3}
     | Term '*' Term                       {Mult <\$> $1 <@> $3}
     | '(' Term ')' {$2} 

Type : 'B' {atLocal dM TyBool}
     | 'U' {atLocal dM TyUnit}
     | 'N' {atLocal dM TyNat}
     | 'A' {atLocal dM TyAny}
     | Type '->' Type {TyFunc <\$> $1 <@> $3 }
     | Type '*' Type {(:*:) <\$> $1 <@> $3} 
     | '(' Type ')' {$2} 
{

parseError (tkn:_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    ]

parseString = parseTokens . alexScanTokens

}
