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
  'def'        { TK_DEF }
  ';'          { TK_DELIM }
  '='          { TK_EQ }
  'if'         { TK_IF }
  'then'       { TK_THEN }
  'else'       { TK_ELSE }
  'true'       { TK_TRUE }
  'false'      { TK_FALSE }
  'unit'       { TK_UNIT }
  ':'          { TK_COLON }
  'Bool'       { TK_B }
  'U'          { TK_U }
  '->'         { TK_ARROW }
  varid        { TK_STRING $$ }

%right '.' '->'
%nonassoc varid '(' ')' '\\' ':' 'Bool' 'false' 'true' 'if' 'then' 'else'  
%left APP

%%

Program : TermDef {$1}

TermDef : 'def' varid '=' Term ';' TermDef {($2, $4) : $6}
        | {--} {[]}

Term : '\\' varid ':' Type '.' Term  {Abs $2 $4 $6} 
     | varid               {Var $1} 
     | Term Term %prec APP {App $1 $2}
     | 'if' Term 'then' Term 'else' Term {If $2 $4 $6}
     | 'true' {TT}
     | 'false' {FF}
     | 'unit' {UU}
     | '(' Term ')' {$2} 

Type : 'Bool' { TyBool }
     | 'U' {TyUnit}
     | Type '->' Type { TyFunc $1 $3 }
     | '(' Type ')' {$2} 
{

parseError (tkn:_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    ]

parseString = parseTokens . alexScanTokens

}
