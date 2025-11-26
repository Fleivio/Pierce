{
module LambdaTyped.Parser(parseString) where
import LambdaTyped.Lexer
import LambdaTyped.Ast
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '\\'         { TK_LAM }
  '.'          { TK_DOT }
  '('          { TK_LPAREN }
  ')'          { TK_RPAREN }
  'let'        { TK_LET }
  'in'         { TK_IN }
  '='          { TK_EQ }
  'if'         { TK_IF }
  'then'       { TK_THEN }
  'else'       { TK_ELSE }
  'true'       { TK_TRUE }
  'false'      { TK_FALSE }
  'unit'       { TK_UNIT }
  ':'          { TK_COLON }
  'B'          { TK_B }
  'U'          { TK_U }
  'N'          { TK_N }
  'A'          { TK_A }
  '->'         { TK_ARROW }
  '+'          { TK_SUM }
  '*'          { TK_PROD }
  '{'          { TK_LCBRACK }
  '}'          { TK_RCBRACK }
  'fst'        { TK_FST }
  'snd'        { TK_SND }
  ','          { TK_COMMA }
  'iszero'     { TK_IS_ZERO }
  varid        { TK_STRING $$ }
  num          { TK_NUM $$ }


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

Term : '\\' varid ':' Type '.' Term %shift {Abs $2 $4 $6} 
     | 'let' varid '=' Term 'in' Term {Let $2 $4 $6}
     | varid               {Var $1} 
     | num               {Numb $1} 
     | Term Term %prec APP {App $1 $2}
     | 'if' Term 'then' Term 'else' Term {If $2 $4 $6}
     | 'true' {TT}
     | 'false' {FF}
     | 'unit' {UU}
     | 'fst' Term {Fst $2}
     | 'snd' Term {Snd $2}
     | 'iszero' Term {IsZero $2}
     | '{' Term ',' Term '}' {Pair $2 $4}
     | Term '+' Term {Sum $1 $3}
     | Term '*' Term {Mult $1 $3}
     | '(' Term ')' {$2} 

Type : 'B' { TyBool }
     | 'U' {TyUnit}
     | 'N' {TyNat}
     | 'A' {TyAny}
     | Type '->' Type { TyFunc $1 $3 }
     | Type '*' Type {$1 :*: $3} 
     | '(' Type ')' {$2} 
{

parseError (tkn:_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    ]

parseString = parseTokens . alexScanTokens

}
