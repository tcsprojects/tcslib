let __lexer_line = ref 0
let __lexer_character = ref 0

let __def_parse_exception s = failwith ("Parse error: " ^ s)

let __parse_exception = ref __def_parse_exception

let __def_lts_has_header = fun _ -> !__parse_exception "Calling __def_lts_has_header."

let __lts_has_header = ref __def_lts_has_header

let __def_lts_has_start = fun _ -> !__parse_exception "Calling __def_lts_has_start."

let __lts_has_start = ref __def_lts_has_start

let __def_lts_add_node = fun _ _ _ _ -> !__parse_exception "Calling __def_lts_add_node."

let __lts_add_node = ref __def_lts_add_node

let __def_ts_has_header = fun _ -> !__parse_exception "Calling __def_ts_has_header."

let __ts_has_header = ref __def_ts_has_header

let __def_ts_has_start = fun _ -> !__parse_exception "Calling __def_ts_has_start."

let __ts_has_start = ref __def_ts_has_start

let __def_ts_add_node = fun _ _ _ _ -> !__parse_exception "Calling __def_ts_add_node."

let __ts_add_node = ref __def_ts_add_node
