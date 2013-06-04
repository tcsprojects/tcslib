/* File tcsparitygameparser.mly */
%{

  open Tcsgameparserinternal ;;
  
  let parse_player pl =
	if pl >= 0 && pl <= 1 then pl
	else (!__parse_exception ("Unknown Player: " ^ string_of_int pl); pl)
	
%}

%token <int> INT
%token <string> ANN
%token SEMICOLON
%token COMMA
%token EOF
%token PARITY
%token START

%start game             /* the entry point */
%type <unit> game
%%

game:
      header nodelist                 { }
	| header start nodelist           { }
    | nodelist                        { }

header:
      PARITY INT SEMICOLON      { !__pg_has_header $2 }

start:
      START INT SEMICOLON      { !__pg_has_start $2 }

nodelist:
      EOF                      { }
    | node SEMICOLON nodelist  { }
;

node:
      INT INT INT succs      { !__pg_add_node $1 $2 (parse_player $3) $4 "" }
    | INT INT INT succs ANN  { !__pg_add_node $1 $2 (parse_player $3) $4 $5 }
;

succs:
      INT                   { [ $1 ] }
    | INT COMMA succs       { $1::$3 }
;