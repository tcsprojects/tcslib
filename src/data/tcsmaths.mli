module FloatUtils :
  sig
    val factorial : int -> float
    val choose : int -> int -> float
    val sum : int -> int -> (int -> float) -> float
    val prod : int -> int -> (int -> float) -> float
  end
  
module IntUtils :
  sig
    val power : int -> int -> int
    val factorial : int -> int
    val choose : int -> int -> int
    val sum : int -> int -> (int -> int) -> int
    val prod : int -> int -> (int -> int) -> int
  end
  
open Big_int;;  
  
module BigInt :
  sig
    type t
  
	val of_int: int -> t
	val to_int: t -> int
	val to_float: t -> float
	
	val to_string: t -> string
	
	val compare: t -> t -> int
	val equal: t -> t -> bool

	val zero: t
	val one: t
	val two: t
	val three: t
	val four: t
	val five: t
	val six: t
	val seven: t
	val eight: t
	val nine: t
	
	val _mod: t -> t -> t
	val mod_int: t -> int -> int
	
	val add: t -> t -> t
	val add_int: t -> int -> t
	
	val sub: t -> t -> t
	val sub_int: t -> int -> t

	val mult: t -> t -> t
	val mult_int: t -> int -> t

	val div: t -> t -> t
	val div_int: t -> int -> t
	
	val int_power_int: int -> int -> t
	val power: t -> t -> t
	val int_power: int -> t -> t
	val power_int: t -> int -> t

	val even: t -> bool
	val odd: t -> bool

	val sum : int -> int -> (int -> t) -> t
	
	val gcd: t -> t -> t
  end

  
module BigFloat :
  sig
    type t
  
	val of_big_int: BigInt.t -> t
	val of_big_ints: BigInt.t -> BigInt.t -> t
	val of_int: int -> t
	val of_ints: int -> int -> t
	val to_float: t -> float
	
	val format: t -> string
	val format_fraction: t -> string
	
	val floor: t -> BigInt.t

	val compare: t -> t -> int
	val equal: t -> t -> bool
	
	val min: t -> t -> t
	val max: t -> t -> t
	
	val zero: t
	val one: t
	
	val add: t -> t -> t
	val sub: t -> t -> t
	val mult: t -> t -> t
	val div: t -> t -> t
  end
 
	
	
module RandomUtils :
  sig
	exception Not_Enough_Choices
	
	(* get_pairwise_different_from_range n l h selects n pairwise
	   different numbers from the range {l,...,h}
	
	   runs in O(n)
	
	   it throws a Not_Enough_Choices exception if n > h-l+1
	
	   it expects the random generator to be initialised already!
	*)
	val get_pairwise_different_from_range : int -> int -> int -> int array
	
	val randrange: int -> int -> int
	
	val randseed: unit -> int
  end
  
module MathField :
  sig
	type 'a field = (
		'a * (* zero *)
		'a * (* one *)
		('a -> 'a -> 'a) * (* add *)
		('a -> 'a) * (* neg *)
		('a -> 'a -> 'a) * (* sub *)
		('a -> 'a -> 'a) * (* mul *)
		('a -> 'a) * (* inv *)
		('a -> 'a -> 'a) * (* div *)
		('a -> int -> 'a) * (* power *)
		('a -> 'a -> int) * (* compare *)
		(int -> int -> 'a) (* int to *)
	)

	val float_field: float field

	type rational = Big_int.big_int * Big_int.big_int
	val rational_field: rational field
  end
  