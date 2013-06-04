let __lexer_line = ref 0
let __lexer_character = ref 0

let __def_parse_exception s = failwith ("Parse error: " ^ s)

let __parse_exception = ref __def_parse_exception

let __def_pg_has_header = fun _ -> !__parse_exception "Calling __def_pg_has_header."

let __pg_has_header = ref __def_pg_has_header

let __def_pg_has_start = fun _ -> !__parse_exception "Calling __def_pg_has_start."

let __pg_has_start = ref __def_pg_has_start

let __def_pg_add_node = fun _ _ _ _ _ -> !__parse_exception "Calling __def_pg_add_node."

let __pg_add_node = ref __def_pg_add_node

let __def_pgsol_has_header = fun _ -> !__parse_exception "Calling __def_pgsol_has_header."

let __pgsol_has_header = ref __def_pgsol_has_header

let __def_pgsol_add_node = fun _ _ _ -> !__parse_exception "Calling __def_pgsol_add_node."

let __pgsol_add_node = ref __def_pgsol_add_node
