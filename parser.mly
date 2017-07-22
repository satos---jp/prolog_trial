%{
	open Syntax
%}


%token <string> CONST
%token <string> PARAM
%token LPAR RPAR
%token COLON COMMA
%token IF

%token SHARP
%token LOAD

%token BLPAR
%token BRPAR
%token QUOTE
%token <string> FILENAME
%token EOF
%token LIST
%token VERT
%token CUT

%start repl decls
%type <Syntax.direct> repl
%type <Syntax.decl list> decls

%%

repl:
	| directive COLON { $1 }
	| funcs COLON      { Query($1) }
	| EOF  { Quit }

directive:
	| BLPAR QUOTE FILENAME QUOTE BRPAR { Load($3) }
	| LIST { List }	


decls:
	| EOF { [] }
	| decl decls { $1 :: $2 }

decl:
	| func COLON          { ($1,[]) }
	| func IF funcs COLON { ($1,$3) }

funcs:
	| func_with_cut      { [$1] }
	| func_with_cut COMMA funcs { $1 :: $3 }

func_with_cut:
	| func { Func($1) }

func:
	| CONST LPAR pattern funcrest { ($1,$3 :: $4) }
	| CONST { ($1,[]) }

funcrest:
	| RPAR  { [] }
	| COMMA pattern funcrest { $2 :: $3 }

pattern:
	| param { $1 }
	| const { $1 }
	| concat { $1 }
	| nilpat { $1 }
	| listpat { $1 }


const:
	| CONST { Const($1,[]) }
	| CONST LPAR pattern constrest { Const($1,$3 :: $4) }

constrest:
	| RPAR { [] }
	| COMMA pattern constrest { $2 :: $3 }

param:
	| PARAM { Param($1) }

concat:
	| BLPAR pattern VERT pattern BRPAR { Const("@cons", [$2; $4]) }

nilpat:
	| BLPAR BRPAR { Const("@nil",[]) }


listpat:
	| BLPAR listrest { $2 }
	
listrest:
	| pattern BRPAR { Const("@cons", [$1; Const("@nil",[])]) }
	| pattern COMMA listrest { Const("@cons", [$1; $3]) }




