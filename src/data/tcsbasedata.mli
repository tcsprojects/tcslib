module OptionUtils : sig

	val is_none: 'a option -> bool
	val is_some: 'a option -> bool
	val get_some: 'a option -> 'a
	val map_some : 'a option -> ('a -> 'b) -> 'b option
	val resolve : 'a option -> ('a -> 'b) -> 'b -> 'b

end


module Tuple : sig

	type ('a, 'b) t = 'a * 'b
	type 'a s = ('a, 'a) t
	
	val assemble: 'a -> 'b -> ('a, 'b) t
	val fst: ('a, 'b) t -> 'a
	val snd: ('a, 'b) t -> 'b
	val get: 'a s -> int -> 'a

end


module Triple : sig

	type ('a, 'b, 'c) t = 'a * 'b * 'c
	type 'a s = ('a, 'a, 'a) t
	
	val assemble: 'a -> 'b -> 'c -> ('a, 'b, 'c) t
	val fst: ('a, 'b, 'c) t -> 'a
	val snd: ('a, 'b, 'c) t -> 'b
	val trd: ('a, 'b, 'c) t -> 'c
	val get: 'a s -> int -> 'a

end


module Comparators : sig

	type 'a comparator = 'a -> 'a -> int

	val default: 'a comparator
	val nocompare: 'a comparator
	val product: 'a comparator -> 'b comparator -> ('a * 'b) comparator
	val product3: 'a comparator -> 'b comparator -> 'c comparator -> ('a * 'b * 'c) comparator
	val product4: 'a comparator -> 'b comparator -> 'c comparator -> 'd comparator -> ('a * 'b * 'c * 'd) comparator
	val product5: 'a comparator -> 'b comparator -> 'c comparator -> 'd comparator -> 'e comparator -> ('a * 'b * 'c * 'd * 'e) comparator
	val inner_product: 'a comparator -> 'a comparator -> 'a comparator
	val option_compare: 'a comparator -> ('a option) comparator
	val by_map: 'b comparator -> ('a -> 'b) -> 'a comparator
	val by_default_map: ('a -> 'b) -> 'a comparator

end;;


module Formators : sig

	type 'a formator = 'a -> string

	val def_int: int formator
	val def_string: string formator
	val def_bool: bool formator
	
	val product: 'a formator -> 'b formator -> ('a * 'b) formator
	val product3: 'a formator -> 'b formator -> 'c formator -> ('a * 'b * 'c) formator
	val option_format: string -> string -> 'a formator -> ('a option) formator
	
end;;


module Iterators :
  sig
    type 'a iterator = ('a -> bool) -> unit
    type 'a full_iterator = ('a -> unit) -> unit
	val is_empty : 'a iterator -> bool
	val cardinal: 'a iterator -> int
	val first: 'a iterator -> 'a
    val iter : 'a iterator -> 'a full_iterator
	val map: ('a -> 'b) -> 'a iterator -> 'b iterator
    val exists : 'a iterator -> ('a -> bool)  -> bool
    val forall : 'a iterator -> ('a -> bool) -> bool
    val fold : 'a iterator -> ('a -> 'c -> 'c) -> 'c -> 'c
	val filter : ('a -> bool) -> 'a iterator -> 'a iterator
    val to_list : 'a iterator -> 'a list
    val of_list : 'a list -> 'a iterator
	val explicit : 'a iterator -> 'a iterator
	val singleton: 'a -> 'a iterator
    val to_array : 'a iterator -> 'a array
    val of_array : 'a array -> 'a iterator
    val of_full_iterator: 'a full_iterator -> 'a iterator
	val second_if_first_empty: 'a iterator -> 'a iterator -> 'a iterator
	val attach: 'a iterator -> 'a iterator -> 'a iterator
	val flatten: ('a iterator) iterator -> 'a iterator
	val product: 'a iterator -> 'b iterator -> ('a * 'b) iterator
	val depend_product: 'a iterator -> ('a -> 'b iterator) -> ('a * 'b) iterator
	val mapped_product: 'a iterator -> ('a -> 'b iterator) -> 'b iterator
	val option_iterator: 'a iterator -> ('a option) iterator
  end


module Enumerators :
  sig
	type ('a, 'b) t = 'b -> ('b * 'a) option
	type 'a enumerator
	
	val make: ('a, 'b) t -> 'b -> 'a enumerator
	
	val empty : 'a enumerator -> bool
	val head : 'a enumerator -> 'a
	val tail : 'a enumerator -> 'a enumerator
	val next: 'a enumerator -> 'a enumerator * 'a
	val to_iterator: 'a enumerator -> 'a Iterators.iterator
	val of_array: 'a array -> 'a enumerator
	val of_list: 'a list -> 'a enumerator
	val singleton: 'a -> 'a enumerator
	val map: ('a -> 'b) -> 'a enumerator -> 'b enumerator
	val to_list: 'a enumerator -> 'a list
end

module Domain :
  sig
    type 'a t
    
    val make: 'a Comparators.comparator -> 'a Formators.formator -> 'a t
    
    val compare: 'a t -> 'a Comparators.comparator
    val format: 'a t -> 'a Formators.formator
    
    val product: 'a t -> 'b t -> ('a * 'b) t
    
    val option_domain: 'a t -> ('a option) t
end
  

module UniqueIntegerAssignment :
  sig
    type t
    val new_assignment: unit -> t
	val assign: t -> int
	val release: t -> int -> unit
  end
  
  
module CompRef :
  sig
	type 'a compref
	
	val newref: 'a -> 'a compref
	val freeref: 'a compref -> unit
	val getref: 'a compref -> 'a
	val setref: 'a compref -> 'a -> unit
	val compare: 'a compref -> 'a compref -> int
	val equal: 'a compref -> 'a compref -> bool
  end
  
  