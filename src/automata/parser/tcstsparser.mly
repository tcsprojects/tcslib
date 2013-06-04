/* File tcstsparser.mly */
%{

  open Tcstransitionsysparserinternal ;;
  
%}

%token <int> INT
%token <string> ANN
%token <string> STRING
%token SEMICOLON
%token COMMA
%token EOF
%token TS
%token START

%start ts             /* the entry point */
%type <unit> ts
%%

ts:
      header nodelist                 { }
	| header start nodelist           { }
    | nodelist                        { }

header:
      TS INT SEMICOLON      { !__ts_has_header $2 }

start:
      START INT SEMICOLON      { !__ts_has_start $2 }

nodelist:
      EOF                      { }
    | node SEMICOLON nodelist  { }
;

node:
		INT { !__ts_add_node $1 [] [] None }
	|	INT transitions { !__ts_add_node $1 $2 [] None }
	|	INT propositions { !__ts_add_node $1 [] $2 None }
	|	INT ANN { !__ts_add_node $1 [] [] (Some $2) }
	|	INT transitions propositions { !__ts_add_node $1 $2 $3 None }
	|	INT transitions ANN { !__ts_add_node $1 $2 [] (Some $3) }
	|	INT propositions ANN { !__ts_add_node $1 [] $2 (Some $3) }
	|	INT transitions propositions ANN { !__ts_add_node $1 $2 $3 (Some $4) }
;

transitions:
		INT 					{ [ $1 ] }
	|	INT COMMA transitions  { $1::$3 }
;

propositions:
      STRING                   	    { [ $1 ] }
    | STRING COMMA propositions       { $1::$3 }
;