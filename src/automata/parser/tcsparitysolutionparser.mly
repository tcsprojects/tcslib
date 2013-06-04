/* File tcsparitysolutionparser.mly */
%{

  open Tcsgameparserinternal ;;
  
  let parse_player pl =
	if pl >= 0 && pl <= 1 then pl
	else (!__parse_exception ("Unknown Player: " ^ string_of_int pl); pl)
	
%}

%token <int> INT
%token SEMICOLON
%token EOF
%token PARITYSOL

%start sol             /* the entry point */
%type <unit> sol
%%

sol:
      header nodelist                 { }
    | nodelist                        { }

header:
      PARITYSOL INT SEMICOLON      { !__pgsol_has_header $2 }

nodelist:
      EOF                      { }
    | node SEMICOLON nodelist  { }
;

node:
      INT INT      { !__pgsol_add_node $1 $2 None }
    | INT INT INT  { !__pgsol_add_node $1 $2 (Some $3) }
;