{
module LambUnt.Parser(parseString) where
import LambUnt.Lexer
import LambUnt.Named
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '\\'         { TK_LAM }
  '.'          { TK_DOT }
  '('          { TK_LPAREN }
  ')'          { TK_RPAREN }
  varid        { TK_STRING $$ }

%right '.'
%nonassoc varid '(' ')' '\\'
%left APP

%%

Program : Term {$1}

Term : '\\' varid AbsBody  {Abs $2 $3} 
     | varid               {Var $1} 
     | Term Term %prec APP {App $1 $2}
     | '(' Term ')' {$2} 
     
AbsBody : '.' Term { $2 } 
        | varid AbsBody {Abs $1 $2}
{

parseError (tkn:_)
  = error $ unlines [
     "\n\nPARSING ERROR:" 
    , "Unexpected: " ++ show tkn
    ]

parseString = parseTokens . alexScanTokens

}
