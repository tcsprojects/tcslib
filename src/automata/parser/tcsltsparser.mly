/* File tcsltsparser.mly */
%{

  open Tcstransitionsysparserinternal ;;
  
%}

%token <int> INT
%token <string> ANN
%token <string> STRING
%token SEMICOLON
%token COMMA
%token COLON
%token EOF
%token LTS
%token START

%start lts             /* the entry point */
%type <unit> lts
%%

lts:
      header nodelist                 { }
	| header start nodelist           { }
    | nodelist                        { }

header:
      LTS INT SEMICOLON      { !__lts_has_header $2 }

start:
      START INT SEMICOLON      { !__lts_has_start $2 }

nodelist:
      EOF                      { }
    | node SEMICOLON nodelist  { }
;

node:
		INT { !__lts_add_node $1 [] [] None }
	|	INT transitions { !__lts_add_node $1 $2 [] None }
	|	INT propositions { !__lts_add_node $1 [] $2 None }
	|	INT ANN { !__lts_add_node $1 [] [] (Some $2) }
	|	INT transitions propositions { !__lts_add_node $1 $2 $3 None }
	|	INT transitions ANN { !__lts_add_node $1 $2 [] (Some $3) }
	|	INT propositions ANN { !__lts_add_node $1 [] $2 (Some $3) }
	|	INT transitions propositions ANN { !__lts_add_node $1 $2 $3 (Some $4) }
;

transitions:
		STRING COLON INT 					{ [ ($1,$3) ] }
	|	STRING COLON INT COMMA transitions  { ($1,$3)::$5 }
;

propositions:
      STRING                   	    { [ $1 ] }
    | STRING COMMA propositions       { $1::$3 }
;