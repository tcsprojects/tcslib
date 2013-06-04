val __lexer_line: int ref
val __lexer_character: int ref

val __def_parse_exception: string -> unit

val __parse_exception: (string -> unit) ref

val __def_lts_has_header: int -> unit

val __lts_has_header: (int -> unit) ref

val __def_lts_has_start: int -> unit

val __lts_has_start: (int -> unit) ref

val __def_lts_add_node: int -> (string * int) list -> string list -> string option -> unit

val __lts_add_node: (int -> (string * int) list -> string list -> string option -> unit) ref

val __def_ts_has_header: int -> unit

val __ts_has_header: (int -> unit) ref

val __def_ts_has_start: int -> unit

val __ts_has_start: (int -> unit) ref

val __def_ts_add_node: int -> int list -> string list -> string option -> unit

val __ts_add_node: (int -> int list -> string list -> string option -> unit) ref
