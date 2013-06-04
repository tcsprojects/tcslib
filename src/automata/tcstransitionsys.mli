open Tcstiming
open Tcsbasedata
open Tcsset

type ('s, 'l, 'p) initlts =
	('s *
	 ('s -> 'p list) *
	 ('s -> ('l * 's) list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('l -> string) *
	 ('p -> string))

val get_timed_initlts:
	('s, 'l, 'p) initlts -> SimpleTiming.timing_object -> ('s, 'l, 'p) initlts

val get_cached_initlts:
	('s, 'l, 'p) initlts -> ('s, 'l, 'p) initlts

val get_int_cached_initlts :
  ('s, 'l, 'p) initlts -> ('s -> int -> unit) ->
  (int, 'l, 'p) initlts * ('s -> int) * (int -> 's)

type explicit_lts =
	string array * (* propositions *)
	string array * (* labels *)
	(int array * (* propositions *)
	 (int * int) array * (* label,node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initlts = int * explicit_lts

val build_explicit_initlts:
	(int, 'l, 'p) initlts -> (int -> 's) -> ('s -> string option) -> (int -> unit) -> explicit_initlts

val print_explicit_lts: explicit_lts -> (string -> unit) -> unit

val print_explicit_initlts: explicit_initlts -> (string -> unit) -> unit

type ('s, 'p) initts =
	('s *
	 ('s -> 'p list) *
	 ('s -> 's list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('p -> string))

val get_timed_initts:
	('s, 'p) initts -> SimpleTiming.timing_object -> ('s, 'p) initts

val get_cached_initts:
	('s, 'p) initts -> ('s, 'p) initts

val get_int_cached_initts :
  ('s, 'p) initts -> ('s -> int -> unit) ->
  (int, 'p) initts * ('s -> int) * (int -> 's)

type explicit_ts =
	string array * (* propositions *)
	(int array * (* propositions *)
	 int array * (* node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initts = int * explicit_ts

val build_explicit_initts:
	(int, 'p) initts -> (int -> 's) -> ('s -> string option) -> (int -> unit) -> explicit_initts

val print_explicit_ts: explicit_ts -> (string -> unit) -> unit

val print_explicit_initts: explicit_initts -> (string -> unit) -> unit
