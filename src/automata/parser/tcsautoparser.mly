/* File tcsautoparser.mly */
%{

  open Tcsautomataparserinternal ;;
  
%}

%token <int> INT
%token <string> ANN
%token SEMICOLON
%token COMMA
%token BRACKETOPEN
%token BRACKETCLOSE
%token ALPHABETCALL
%token ALPHABETRET
%token UNDERSCORE
%token EOF
%token AUTOMATON
%token ALPHABET
%token STATES
%token INITIAL
%token TRANSITIONS
%token STACK
%token EPSILON

%start auto             /* the entry point */
%type <unit> auto
%%

auto:
		automaton alphabet states initial transitions EOF { }
	|	automaton alphabet states transitions EOF { }
	|	automaton alphabet states stack initial transitions EOF { }
	|	automaton alphabet states stack transitions EOF { }

automaton: AUTOMATON ANN SEMICOLON { !__automaton_type $2 }
		
alphabet:
		ALPHABET INT SEMICOLON 				{ !__automaton_int_alphabet $2 }
	|	ALPHABET SEMICOLON alphabetlist 	{ }
	
alphabetlist:
		alphabetitem SEMICOLON					{ }
	|	alphabetitem SEMICOLON alphabetlist		{ }
	
alphabetitem:
		INT ANN { !__automaton_alphabet_add $1 $2 AlphabetInternal }
	|	INT ALPHABETCALL ANN { !__automaton_alphabet_add $1 $3 AlphabetCall }
	|	INT ALPHABETRET ANN { !__automaton_alphabet_add $1 $3 AlphabetRet }

stack:
		STACK INT SEMICOLON 				{ !__automaton_int_stack $2 }
	|	STACK SEMICOLON stacklist 	{ }
	
stacklist:
		stackitem SEMICOLON					{ }
	|	stackitem SEMICOLON stacklist		{ }
	
stackitem: INT ANN { !__automaton_stack_add $1 $2 }

states: STATES SEMICOLON statelist		 	{ }
	
statelist:
		stateitem SEMICOLON					{ }
	|	stateitem SEMICOLON statelist		{ }
	
stateitem:
		INT INT 		{ !__automaton_state_add $1 $2 None }
	|	INT INT ANN		{ !__automaton_state_add $1 $2 (Some $3) }

initial: INITIAL INT SEMICOLON				{ !__automaton_initial_state $2 }

transitions: TRANSITIONS SEMICOLON transitionlist		 	{ }
	
transitionlist:
		transitionitemdecider SEMICOLON					{ }
	|	transitionitemdecider SEMICOLON transitionlist		{ }

transitionitemdecider:
	  transitionitem { }
	| transitionitemcall { }
	| transitionitemret { }
	| transitionitemepsilon {}
	
transitionitem:
		INT INT succs { !__automaton_transition_add $1 $2 $3 }

transitionitemcall:
		INT INT pairsuccs { !__automaton_transition_call_add $1 $2 $3 }

transitionitemret:
		INT INT INT succs { !__automaton_transition_ret_add $1 $3 (Some $2) $4 }
	|	INT UNDERSCORE INT succs { !__automaton_transition_ret_add $1 $3 None $4 }

transitionitemepsilon:
		INT EPSILON succs { !__automaton_transition_epsilon_add $1 $3 }
	
succs: 
      INT                   { [ $1 ] }
    | INT COMMA succs       { $1::$3 }

pairsuccs: 
      BRACKETOPEN INT COMMA INT BRACKETCLOSE                   { [($2,$4)] }
    | BRACKETOPEN INT COMMA INT BRACKETCLOSE COMMA pairsuccs   { ($2,$4)::$7 }
    | BRACKETOPEN INT BRACKETCLOSE                             { [($2,$2)] }
    | BRACKETOPEN INT BRACKETCLOSE COMMA pairsuccs             { ($2,$2)::$5 }
