type alphabet_item_type = AlphabetInternal | AlphabetCall | AlphabetRet

let __lexer_line = ref 0
let __lexer_character = ref 0

let __def_parse_exception s = failwith ("Parse error: " ^ s)

let __parse_exception = ref __def_parse_exception


let __def_automaton_type = fun _ -> !__parse_exception "Calling __def_automaton_type."

let __automaton_type = ref __def_automaton_type

let __def_automaton_int_alphabet = fun _ -> !__parse_exception "Calling __def_automaton_int_alphabet."

let __automaton_int_alphabet = ref __def_automaton_int_alphabet

let __def_automaton_alphabet_add = fun _ _ _ -> !__parse_exception "Calling __def_automaton_alphabet_add."

let __automaton_alphabet_add = ref __def_automaton_alphabet_add

let __def_automaton_int_stack = fun _ -> !__parse_exception "Calling __def_automaton_int_stack."

let __automaton_int_stack = ref __def_automaton_int_stack

let __def_automaton_stack_add = fun _ _ -> !__parse_exception "Calling __def_automaton_stack_add."

let __automaton_stack_add = ref __def_automaton_stack_add

let __def_automaton_state_add = fun _ _ _ -> !__parse_exception "Calling __def_automaton_state_add."

let __automaton_state_add = ref __def_automaton_state_add

let __def_automaton_initial_state = fun _ -> !__parse_exception "Calling __def_automaton_initial_state."

let __automaton_initial_state = ref __def_automaton_initial_state

let __def_automaton_transition_add = fun _ _ _ -> !__parse_exception "Calling __def_automaton_transition_add."

let __automaton_transition_add = ref __def_automaton_transition_add

let __def_automaton_transition_epsilon_add = fun _ _ -> !__parse_exception "Calling __def_automaton_transition_epsilon_add."

let __automaton_transition_epsilon_add = ref __def_automaton_transition_epsilon_add

let __def_automaton_transition_call_add = fun _ _ _ -> !__parse_exception "Calling __def_automaton_transition_call_add."

let __automaton_transition_call_add = ref __def_automaton_transition_call_add

let __def_automaton_transition_ret_add = fun _ _ _ _ -> !__parse_exception "Calling __def_automaton_transition_ret_add."

let __automaton_transition_ret_add = ref __def_automaton_transition_ret_add

