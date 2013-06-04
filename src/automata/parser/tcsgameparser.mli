open Tcsgames

exception GameParserException of string * int * int

exception GameParserCustomException of string

val parse_parity_game: (int -> unit) -> (int -> unit) ->
                       (int -> int -> int -> int list -> string -> unit) ->
					   (unit -> 'a) ->
					   in_channel ->
					   'a

val parse_explicit_pg: in_channel -> explicit_pg
					   
val parse_explicit_initpg: in_channel -> explicit_initpg

val parse_parity_solution: (int -> unit) ->
                           (int -> int -> int option -> unit) ->
						   (unit -> 'a) ->
						   in_channel ->
						   'a

val parse_explicit_parity_solution: in_channel -> explicit_pg_solution * explicit_pg_strategy

