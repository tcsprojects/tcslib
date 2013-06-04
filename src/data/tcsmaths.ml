module FloatUtils = struct
	
	let factorial n =
		let a = ref 1.0 in
		for i = 2 to n do
			a := !a *. (float i)
		done;
		!a
		
	let choose n k =
		if (k > n) || (k < 0) then 0.0 else (
			let a = ref 1.0 in
			for i = 1 to k do
				a := !a *. float (n - i + 1) /. float i
			done;
			!a
		)

	let sum l u f =
		let a = ref 0.0 in
		for i = l to u do
			a := !a +. f i
		done;
		!a
		
	let prod l u f =
		let a = ref 1.0 in
		for i = l to u do
			a := !a *. f i
		done;
		!a
	
end;;


module IntUtils = struct

	let power i j =
		truncate (float i ** float j)
		
	let factorial n =
		let a = ref 1 in
		for i = 2 to n do
			a := !a * i
		done;
		!a
		
	let choose n k =
		if (k > n) || (k < 0) then 0 else (
			let a = ref 1 in
			for i = 1 to k do
				a := !a * (n - i + 1) / i
			done;
			!a
		)
		
	let sum l u f =
		let a = ref 0 in
		for i = l to u do
			a := !a + f i
		done;
		!a
		
	let prod l u f =
		let a = ref 1 in
		for i = l to u do
			a := !a * f i
		done;
		!a

end;;

open Big_int;;

module BigInt = struct

	type t = big_int

	let of_int = big_int_of_int
	let to_int = int_of_big_int
	let to_float = float_of_big_int
	
	let to_string b = string_of_big_int b
	
	let compare = compare_big_int
	let equal = eq_big_int

	let zero = zero_big_int
	let one = unit_big_int
	let two = of_int 2
	let three = of_int 3
	let four = of_int 4
	let five = of_int 5
	let six = of_int 6
	let seven = of_int 7
	let eight = of_int 8
	let nine = of_int 9
	
	let _mod b1 b2 = mod_big_int b1 b2
	let mod_int b i = to_int (_mod b (of_int i))
	
	let add b1 b2 = add_big_int b1 b2
	let add_int b i = add_int_big_int i b
	
	let sub b1 b2 = sub_big_int b1 b2
	let sub_int b i = add_int b (-i)
	
	let mult b1 b2 = mult_big_int b1 b2
	let mult_int b i = mult_int_big_int i b
	
	let div b1 b2 = div_big_int b1 b2
	let div_int b i = div_big_int b (of_int i)
	
	let int_power_int i1 i2 = power_int_positive_int i1 i2

	let even b = mod_int b 2 = 0
	let odd b = mod_int b 2 = 1

	let sum l u f =
		let a = ref zero in
		for i = l to u do
			a := add !a (f i)
		done;
		!a
		
	let gcd a b =
		gcd_big_int a b
		
(*
	let power i j =
		truncate (float i ** float j)
		
	let factorial n =
		let a = ref 1 in
		for i = 2 to n do
			a := !a * i
		done;
		!a
		
	let choose n k =
		if (k > n) || (k < 0) then 0 else (
			let a = ref 1 in
			for i = 1 to k do
				a := !a * (n - i + 1) / i
			done;
			!a
		)
		
	let prod l u f =
		let a = ref 1 in
		for i = l to u do
			a := !a * f i
		done;
		!a
*)
end;;

module BigFloat = struct

	type t = big_int * big_int
	
	let normalize (a,b) =
		let (ca,cb) = (BigInt.compare a BigInt.zero, BigInt.compare b BigInt.zero) in
		if cb = 0
		then (BigInt.zero, BigInt.zero)
		else if ca = 0
		then (BigInt.zero, BigInt.one)
		else let sgn = ca * cb in
			 let (pa,pb) = ((if ca < 0 then BigInt.mult_int a (-1) else a), (if cb < 0 then BigInt.mult_int b (-1) else b)) in
			 let g = BigInt.gcd pa pb in
			 ((if sgn >= 0 then BigInt.div pa g else BigInt.mult_int (BigInt.div pa g) (-1)), BigInt.div pb g)
			 
	let of_big_int b =
		normalize (b, BigInt.one)
	
	let of_big_ints d n =
		normalize (d, n)

	let zero = of_big_int (BigInt.zero)
	let one = of_big_int (BigInt.one)

	let of_int i =
		of_big_int (BigInt.of_int i)
		
	let of_ints n d =
		normalize (BigInt.of_int n, BigInt.of_int d)
		
	let floor (a, b) =
		BigInt.div a b
		
	let to_float (a, b) =
		BigInt.to_float a /. BigInt.to_float b
	
	let format f =
		Printf.sprintf "%.8f" (to_float f)
		
	let format_fraction (n,d) =
		if BigInt.compare d BigInt.one = 0
		then BigInt.to_string n
		else if BigInt.compare n BigInt.zero = 0
		then "0"
		else BigInt.to_string n ^ "/" ^ BigInt.to_string d
		
	let add (a,b) (c,d) =
		normalize (BigInt.add (BigInt.mult a d) (BigInt.mult c b), BigInt.mult b d)
		
	let sub (a,b) (c,d) =
		normalize (BigInt.sub (BigInt.mult a d) (BigInt.mult c b), BigInt.mult b d)

	let mult (a,b) (c,d) =
		normalize (BigInt.mult a c, BigInt.mult b d)

	let div (a,b) (c,d) =
		normalize (BigInt.mult a d, BigInt.mult b c)

	let compare x y =
		BigInt.compare (fst (sub x y)) BigInt.zero
		
	let equal (a,b) (c,d) =
		BigInt.equal a c && BigInt.equal b d
		
	let min x y =
		if compare x y >= 0 then y else x	
	
	let max x y =
		if compare x y >= 0 then x else y

end;;


module RandomUtils = struct
	exception Not_Enough_Choices
	
	let get_pairwise_different_from_range n low high =
	  let d = high - low + 1 in
	  if n > d then raise Not_Enough_Choices;
	  let m = d - n + 1 in
	  let a = Array.init n (fun _ -> Random.int m) in
	  Array.sort compare a;
	  for i = 0 to n - 1 do
	    a.(i) <- a.(i) + i + low
	  done;
	  a
	
	let randrange i j = i + Random.int (j - i + 1)
	
	external randseed: unit -> int = "caml_sys_random_seed"
end;;


module MathField = struct
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
	
	let float_field: float field = (
		0.0,
		1.0,
		(fun x y -> x +. y),
		(fun x -> -. x),
		(fun x y -> x -. y),
		(fun x y -> x *. y),
		(fun x -> 1.0 /. x),
		(fun x y -> x /. y),
		(fun x y -> x ** (float y)),
		(fun x y -> compare x y),
		(fun x y -> (float x) /. (float y))
	)
	
	open Big_int
	
	type rational = big_int * big_int
	
	let rational_field: rational field =
		let normalize (a, b) =
			let (n, d) =
				if eq_big_int b zero_big_int then failwith "division by zero"
				else if gt_big_int b zero_big_int then (a, b)
				else (minus_big_int a, minus_big_int b)
			in
			let gcd = gcd_big_int n d in
			(div_big_int n gcd, div_big_int d gcd)
		in (
		(zero_big_int, unit_big_int),
		(unit_big_int, unit_big_int),
		(fun (a, b) (x, y) ->
			normalize ((add_big_int (mult_big_int a y) (mult_big_int x b)),
			           (mult_big_int b y))
		),
		(fun (a, b) -> (minus_big_int a, b)),
		(fun (a, b) (x, y) ->
			normalize ((sub_big_int (mult_big_int a y) (mult_big_int x b)),
					   (mult_big_int b y))
		),
		(fun (a, b) (x, y) -> normalize ((mult_big_int a x), (mult_big_int b y))),
		(fun (a, b) -> normalize (b, a)),
		(fun (a, b) (x, y) -> normalize ((mult_big_int a y), (mult_big_int b x))),
		(fun (a, b) k ->
			let k' = abs k in
			let n = power_big_int_positive_int a k' in
			let d = power_big_int_positive_int b k' in
			normalize (if k' >= 0 then (n, d) else (d, n))
		),
		(fun (a, b) (x, y) -> compare_big_int (mult_big_int a y) (mult_big_int b x)),
		(fun a b -> normalize (big_int_of_int a, big_int_of_int b))
	)
end;;