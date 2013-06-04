open Tcsbasedata;;

module AvlTree :
  sig
    type 'a t = Empty | Node of 'a * int * 'a t * 'a t

    val empty : 'a t
    val is_empty : 'a t -> bool
    val singleton : 'a -> 'a t
    val left : 'a t -> 'a t
    val right : 'a t -> 'a t
    val min_elt : 'a t -> 'a
    val max_elt : 'a t -> 'a
    val root : 'a t -> 'a
    val height : 'a t -> int
    val height_join : 'a t -> 'b t -> int
    val create : 'a -> 'a t -> 'a t -> 'a t
    val balance : 'a -> 'a t -> 'a t -> 'a t
    val join : 'a -> 'a t -> 'a t -> 'a t
    val take_min : 'a t -> 'a * 'a t
    val take_max : 'a t -> 'a * 'a t
    val reroot : 'a t -> 'a t -> 'a t
    val take_min_iter : 'a t -> 'a * 'a t
    val take_min_iter2 : 'a t -> 'a option * 'a t
    val take_max_iter : 'a t -> 'a * 'a t
    val take_max_iter2 : 'a t -> 'a option * 'a t
    val iter : ('a -> 'b) -> 'a t -> unit
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val elements : 'a t -> 'a list
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val cardinal : 'a t -> int
    val choose : 'a t -> 'a
  end

module TreeSet : sig
	type 'a t

	val add: 'a -> 'a t -> 'a t
	val min_elt: 'a t -> 'a
	val max_elt: 'a t -> 'a
	val split: 'a -> 'a t -> 'a t * bool * 'a t
	val empty: 'a Comparators.comparator -> 'a t
	val empty_def: 'a t
	val is_empty: 'a t -> bool
	val mem: 'a -> 'a t -> bool
	val singleton: 'a Comparators.comparator -> 'a -> 'a t
	val singleton_def: 'a -> 'a t
	val remove: 'a -> 'a t -> 'a t
	val union: 'a t -> 'a t -> 'a t
	val inter: 'a t -> 'a t -> 'a t
	val diff: 'a t -> 'a t -> 'a t
	val sym_diff: 'a t -> 'a t -> 'a t
	val compare: 'a t -> 'a t -> int
	val equal: 'a t -> 'a t -> bool
	val subset: 'a t -> 'a t -> bool
	val iter: ('a -> unit) -> 'a t-> unit
	val fold: ('a -> 'b -> 'b) -> 'a t-> 'b -> 'b
	val for_all: ('a -> bool) -> 'a t-> bool
	val exists: ('a -> bool) -> 'a t-> bool
	val filter: ('a -> bool) -> 'a t-> 'a t
	val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
	val cardinal: 'a t -> int
	val elements: 'a t -> 'a list
	val of_list: 'a Comparators.comparator -> 'a list -> 'a t
	val of_list_def: 'a list -> 'a t
	val of_array: 'a Comparators.comparator -> 'a array -> 'a t
	val of_array_def: 'a array -> 'a t
	val append_iterator: 'a t -> 'a Iterators.iterator -> 'a t
	val to_iterator: 'a t -> 'a Iterators.iterator
	val power_iterator: ('a Comparators.comparator) -> ('a Iterators.iterator) -> ('a t) Iterators.full_iterator
	val iterate_subsets: 'a t -> ('a t -> unit) -> unit
	val fold_subsets: ('a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val format: ('a Formators.formator) -> ('a t) Formators.formator
	val remove_list_dups : 'a list -> 'a list	
	val choose: 'a t -> 'a
	val get_compare: 'a t -> 'a Comparators.comparator
	val map: ('a -> 'a) -> 'a t -> 'a t
	val map_filter: ('a -> 'a option) -> 'a t -> 'a t
	val map2: 'b Comparators.comparator -> ('a -> 'b) -> 'a t -> 'b t
	val map2_def: ('a -> 'b) -> 'a t -> 'b t
	val map2_filter: 'b Comparators.comparator -> ('a -> 'b option) -> 'a t -> 'b t	
	val sym_diff: 'a t -> 'a t -> 'a t
end

module TreeMap : sig
    type ('k, 'v) t
    
    val empty : 'k Comparators.comparator -> ('k, 'v) t
    val empty_def : ('k, 'v) t
	val singleton : 'k Comparators.comparator -> 'k -> 'v -> ('k, 'v) t
	val singleton_def : 'k -> 'v -> ('k, 'v) t
    val cardinal: ('k, 'v) t -> int
    val is_empty : ('k, 'v) t -> bool
    val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
    val find : 'k -> ('k, 'v) t -> 'v
	val find_opt: 'k -> ('k, 'v) t -> 'v option
    val mem : 'k -> ('k, 'v) t -> bool
    val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
    val iter : ('k -> 'v -> 'unit) -> ('k, 'v) t -> unit
	val to_key_iterator: ('k, 'v) t -> 'k Iterators.iterator
	val to_value_iterator: ('k, 'v) t -> 'v Iterators.iterator
    val for_all : ('k -> 'v -> bool) -> ('k, 'v) t -> bool
    val map : ('v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
    val mapi : ('k -> 'v -> 'w) -> ('k, 'v) t -> ('k, 'w) t
    val fold : ('k -> 'v -> 'a -> 'a) -> ('k, 'v) t -> 'a -> 'a
    val compare :
      ('v -> 'v -> int) -> ('k, 'v) t -> ('k, 'v) t -> int
    val equal :
      ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool
	val array_to_reverse_map: 'a array -> 'a Comparators.comparator -> ('a, int) t
	val format: ('k * 'v) Formators.formator -> (('k, 'v) t) Formators.formator
    val pairs: ('k, 'v) t -> ('k * 'v) list
	val update: 'k -> 'v -> ('v -> 'v) -> ('k, 'v) t -> ('k, 'v) t
end;;

module SubsetSet : sig

	type ('a, 'b) t
	
	val empty: ('a -> 'a -> int) -> ('a, 'b) t
	val support: ('a, 'b) t -> 'a -> int TreeSet.t
	val supersets: ('a, 'b) t -> 'a TreeSet.t -> int TreeSet.t
	val subsets: ('a, 'b) t -> 'a TreeSet.t -> int TreeSet.t
	val disjointsets: ('a, 'b) t -> 'a TreeSet.t -> int TreeSet.t
	val lookup: ('a, 'b) t -> int -> ('a TreeSet.t) * 'b
	val remove_set: ('a, 'b) t -> int -> ('a, 'b) t
	val remove_sets: ('a, 'b) t -> int TreeSet.t -> ('a, 'b) t
	val singleton: ('a -> 'a -> int) -> ('a TreeSet.t * 'b) -> ('a, 'b) t
	val add_subsume_supersets: ('a, 'b) t -> ('a TreeSet.t * 'b) -> ('a, 'b) t
	val add_subsume_subsets: ('a, 'b) t -> ('a TreeSet.t * 'b) -> ('a, 'b) t
	val is_empty: ('a, 'b) t -> bool
	val get_sets: ('a, 'b) t -> int TreeSet.t

end

module IntervalSet :
  sig
    type 'a intervalsetfuncs =
        ('a -> 'a -> int) * ('a -> 'a) * ('a -> 'a) * ('a -> 'a -> int)
        
    type 'a intervalset

    val height : 'a intervalset -> int
    val empty : 'a intervalsetfuncs -> 'a intervalset
    val is_empty : 'a intervalset -> bool
    val mem : 'a -> 'a intervalset -> bool
    val min_elt : 'a intervalset -> 'a
    val max_elt : 'a intervalset -> 'a
    val add : 'a -> 'a intervalset -> 'a intervalset
    val insert : 'a * 'a -> 'a intervalset -> 'a intervalset
    val singleton : 'a intervalsetfuncs -> 'a -> 'a intervalset
    val remove : 'a -> 'a intervalset -> 'a intervalset
    val union : 'a intervalset -> 'a intervalset -> 'a intervalset
    val iter : ('a -> unit) -> 'a intervalset -> unit
    val fold : ('a -> 'b -> 'b) -> 'a intervalset -> 'b -> 'b
    val fold_right : ('a -> 'b -> 'b) -> 'a intervalset -> 'b -> 'b
    val elements : 'a intervalset -> 'a list
    val for_all : ('a -> bool) -> 'a intervalset -> bool
    val exists : ('a -> bool) -> 'a intervalset -> bool
    val filter : ('a -> bool) -> 'a intervalset -> 'a intervalset
    val cardinal : 'a intervalset -> int
    val choose : 'a intervalset -> 'a
    val split : 'a -> 'a intervalset -> 'a intervalset * bool * 'a intervalset
    val inter : 'a intervalset -> 'a intervalset -> 'a intervalset
    val diff : 'a intervalset -> 'a intervalset -> 'a intervalset
    val compare : 'a intervalset -> 'a intervalset -> int
    val equal : 'a intervalset -> 'a intervalset -> bool
    val subset : 'a intervalset -> 'a intervalset -> bool
    val partition : ('a -> bool) -> 'a intervalset -> 'a intervalset * 'a intervalset
    val count : ('a -> bool) -> 'a intervalset -> int

end

module IntervalSetFuncs : sig

	val int_intervalset: int IntervalSet.intervalsetfuncs
	
end


