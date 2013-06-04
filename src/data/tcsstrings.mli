module StringUtils : sig

	val rest_string: string -> int -> string

	val explode: string -> char -> string list

	val implode: string list -> char -> string

	val indent: string -> int -> char -> string
	
	val index2: string -> char -> int

	val break: string -> int -> char -> string
	
	val break_and_indent: string -> int -> int -> string
	
	val last_line_length: string -> int
	
	val fillup: string -> int -> char -> string
	
	val fillup_left: string -> int -> char -> string
	
	val to_char_array: string -> char array

end;;