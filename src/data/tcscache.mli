open Tcsbasedata;;

module FunctionCache :
  sig
    type ('a, 'b) t
    val cache_function_make : ('a -> 'b) -> 'a Comparators.comparator -> ('a, 'b) t
    val cache_function_get : ('a, 'b) t -> ('a -> 'b)
    val cache_function_size : ('a, 'b) t -> int
    val cache_function : ('a -> 'b) -> 'a Comparators.comparator -> ('a -> 'b)

    type ('a, 'b, 'c) t2
    val cache_function2_make : ('a -> 'b -> 'c) -> 'a Comparators.comparator -> 'b Comparators.comparator -> ('a, 'b, 'c) t2
    val cache_function2_get : ('a, 'b, 'c) t2 -> ('a -> 'b -> 'c)
    val cache_function2_size : ('a, 'b, 'c) t2 -> int
    val cache_function2 : ('a -> 'b -> 'c) -> 'a Comparators.comparator -> 'b Comparators.comparator -> ('a -> 'b -> 'c)
  end
  
module RecursiveFunctionCache :
  sig
    type ('a, 'b) t
    val cache_function_make : (('a -> 'b) -> 'a -> 'b) -> 'a Comparators.comparator -> ('a, 'b) t
    val cache_function_get : ('a, 'b) t -> ('a -> 'b)
    val cache_function_size : ('a, 'b) t -> int
    val cache_function : (('a -> 'b) -> 'a -> 'b) -> 'a Comparators.comparator -> ('a -> 'b)
  end

module IntTypeCache :
  sig
	type 'a t
	
	val make: 'a Comparators.comparator -> 'a -> 'a t
	val touch: 'a t -> 'a -> unit
	val encode: 'a t -> 'a -> int
	val decode: 'a t -> int -> 'a
	val compare: 'a t -> int Comparators.comparator
	val iterate: 'a t -> (int * 'a) Iterators.iterator
	val size: 'a t -> int
	val build_recursively: 'a t -> ('a -> 'a Iterators.iterator) -> unit
  end
  
  
(*
module OrderedTypeLinearizer :
  sig
	type 'a assign_func = 'a -> int -> unit
	type 'a compare_func = 'a -> 'a -> int
	type 'a map_func = 'a -> int
	type 'a t

	val new_linearizer: 'a assign_func -> 'a map_func -> 'a compare_func -> 'a t
	val add: 'a -> 'a t -> unit			
	val compare: 'a t -> 'a -> 'a -> int
  end
  
module OrderedTypeHash :
  sig
	type 'a t
	
	val new_hash: ((int -> int -> int) -> 'a -> 'a -> int) -> 'a -> 'a t
	val encode: 'a t -> 'a -> int
	val decode: 'a t -> int -> 'a
	val compare: 'a t -> int -> int -> int
	val size: 'a t -> int
  end
*)