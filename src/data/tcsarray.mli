open Tcsbasedata;;

module ArrayUtils : sig

	val of_rev_list: 'a list -> 'a array
	
	val sort_with_permutations :
	  'a array -> ('a -> 'a -> int) -> 'a array * (int -> int) * (int -> int)
  
	val mem: 'a -> 'a array -> bool
	
	val forall : 'a array -> (int -> 'a -> bool) -> bool

	val exists : 'a array -> (int -> 'a -> bool) -> bool
  
	val filter: ('a -> bool) -> 'a array -> 'a array

	val custom_format: ('a -> string) -> string -> string -> string -> 'a array -> string

	val format: ('a -> string) -> 'a array -> string
			
	val format_plain: ('a -> string) -> 'a array -> string
	
	val custom_formati: (int -> 'a -> string) -> string -> string -> string -> 'a array -> string

	val formati: (int -> 'a -> string) -> 'a array -> string
			
	val format_plaini: (int -> 'a -> string) -> 'a array -> string
	
	val find2: ('a -> bool) -> 'a array -> int -> int
	
	val find: ('a -> bool) -> 'a array -> int
	
	val index_of2: 'a array -> 'a -> int -> int
	
	val index_of: 'a array -> 'a -> int
	
	val find2_last: ('a -> bool) -> 'a array -> int -> int
	
	val find_last: ('a -> bool) -> 'a array -> int
	
	val index_of2_last: 'a array -> 'a -> int -> int
	
	val index_of_last: 'a array -> 'a -> int

	val min_elt_idx: ('a -> 'a -> int) -> 'a array -> int

	val max_elt_idx: ('a -> 'a -> int) -> 'a array -> int

	val min_elt: ('a -> 'a -> int) -> 'a array -> 'a

	val max_elt: ('a -> 'a -> int) -> 'a array -> 'a
	
	val shuffle: 'a array -> 'a array

end;;


module IntArrayUtils : sig

	val custom_format: string -> string -> string -> int array -> string

	val format: int array -> string
			
	val format_plain: int array -> string

end;;


module DynArray : sig
    type 'a t
    type capacity_updater = int -> int -> int
    val default_capacity_updater: capacity_updater
    val get_capacity_updater: 'a t -> capacity_updater
    val set_capacity_updater: 'a t -> capacity_updater -> unit
    val create : 'a -> 'a t
    val init: 'a -> int -> (int -> 'a) -> 'a t
	val copy : 'a t -> 'a t
    val clear : 'a t -> unit
    val length : 'a t -> int
    val insert : 'a t -> int -> 'a -> unit
    val append : 'a t -> 'a t -> unit
    val fill : 'a t -> int -> 'a -> unit
    val delete : 'a t -> int -> unit
    val delete_last: 'a t -> unit
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> unit
    val add : 'a t -> 'a -> unit
    val iter : ('a -> unit) -> 'a t -> unit
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
    val to_array : 'a t -> 'a array
    val to_list : 'a t -> 'a list
    val map: 'b -> ('a -> 'b) -> 'a t -> 'b t
    val map_inplace: ('a -> 'a) -> 'a t -> unit
    val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val push : 'a t -> 'a -> unit
    val pop : 'a t -> 'a
    val delete_disregarding_ordering : 'a t -> int -> unit
    val take_random : 'a t -> 'a
    val to_iterator: 'a t -> 'a Iterators.iterator
    val to_iteratori: 'a t -> (int * 'a) Iterators.iterator
    val append_iterator: 'a t -> 'a Iterators.iterator -> unit
end;;