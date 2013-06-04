module ListUtils : sig

	val custom_format: ('a -> string) -> string -> string -> string -> 'a list -> string

	val format: ('a -> string) -> 'a list -> string
			
	val format_plain: ('a -> string) -> 'a list -> string
	
	val init: int -> (int -> 'a) -> 'a list
	
	val make: int -> 'a -> 'a list
	
	val compare_lists: ('a -> 'a -> int) -> 'a list -> 'a list -> int
	
	val intersperse: 'a -> 'a list -> 'a list
	
	val min_elt: ('a -> 'a -> int) -> 'a list -> 'a
	
	val max_elt: ('a -> 'a -> int) -> 'a list -> 'a
	
	val iteri: (int -> 'a -> unit) -> 'a list -> unit
	
	val filter_map: ('a -> 'b option) -> 'a list -> 'b list
	
end


module IntListUtils : sig

	val custom_format: string -> string -> string -> int list -> string

	val format: int list -> string
			
	val format_plain: int list -> string

end

