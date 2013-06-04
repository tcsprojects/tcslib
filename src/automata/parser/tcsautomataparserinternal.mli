type alphabet_item_type = AlphabetInternal | AlphabetCall | AlphabetRet


val __lexer_line: int ref
val __lexer_character: int ref

val __def_parse_exception: string -> unit

val __parse_exception: (string -> unit) ref


val __def_automaton_type: string -> unit

val __automaton_type: (string -> unit) ref

val __def_automaton_int_alphabet: int -> unit

val __automaton_int_alphabet: (int -> unit) ref

val __def_automaton_alphabet_add: int -> string -> alphabet_item_type -> unit

val __automaton_alphabet_add: (int -> string -> alphabet_item_type -> unit) ref

val __def_automaton_int_stack: int -> unit

val __automaton_int_stack: (int -> unit) ref

val __def_automaton_stack_add: int -> string -> unit

val __automaton_stack_add: (int -> string -> unit) ref

val __def_automaton_state_add: int -> int -> string option -> unit

val __automaton_state_add: (int -> int -> string option -> unit) ref

val __def_automaton_initial_state: int -> unit

val __automaton_initial_state: (int -> unit) ref

val __def_automaton_transition_add: int -> int -> int list -> unit

val __automaton_transition_add: (int -> int -> int list -> unit) ref

val __def_automaton_transition_epsilon_add: int -> int list -> unit

val __automaton_transition_epsilon_add: (int -> int list -> unit) ref

val __def_automaton_transition_call_add: int -> int -> (int * int) list -> unit

val __automaton_transition_call_add: (int -> int -> (int * int) list -> unit) ref

val __def_automaton_transition_ret_add: int -> int -> int option -> int list -> unit

val __automaton_transition_ret_add: (int -> int -> int option -> int list -> unit) ref
