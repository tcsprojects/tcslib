open Tcstransitionsys

exception TransitionSysParserException of string * int * int

exception TransitionSysParserCustomException of string

val parse_lts: (int -> unit) -> (int -> unit) ->
               (int -> (string * int) list -> string list -> string option -> unit) ->
			   (unit -> 'a) ->
			   in_channel ->
			   'a

val parse_explicit_lts: in_channel -> explicit_lts
					   
val parse_explicit_initlts: in_channel -> explicit_initlts

val parse_ts: (int -> unit) -> (int -> unit) ->
              (int -> int list -> string list -> string option -> unit) ->
			  (unit -> 'a) ->
			  in_channel ->
			  'a

val parse_explicit_ts: in_channel -> explicit_ts
					   
val parse_explicit_initts: in_channel -> explicit_initts
