open Tcstiming
open Tcsbasedata
open Tcsset

type 's initpg = ('s *
                  ('s -> 's list) *
                  ('s -> int) *
                  ('s -> bool) *
                  ('s -> string)) *
                 (int option *
                  int option *
                  ('s -> 's -> int) option)
				  
type 's initpg_solution = 's -> bool option

type 's initpg_strategy = 's -> 's option				  
				  
val get_compact_initpg :
  's initpg -> ('s -> bool) -> ('s * int) initpg
  
val get_compact_initpg_by_player :
  's initpg -> bool -> ('s * int) initpg

val get_escaped_initpg :
  's initpg -> 'a -> ('s * 'a) initpg
  
val get_timed_initpg :
  's initpg -> SimpleTiming.timing_object -> 's initpg

val get_cached_initpg :
  's initpg -> 's initpg

val get_int_cached_initpg :
  's initpg -> ('s -> int -> unit) ->
  int initpg * ('s -> int) * (int -> 's)

type explicit_pg = (int * int * int array * string) array

type explicit_initpg = int * explicit_pg

val build_explicit_initpg : int initpg -> (int -> unit) -> explicit_initpg

val print_explicit_pg: explicit_pg -> (string -> unit) -> unit

val print_explicit_initpg: explicit_initpg -> (string -> unit) -> unit

type explicit_pg_solution = int array

type explicit_pg_strategy = int array

