module OptionUtils = struct

	let is_none = function
		None -> true
	|	_ -> false
	
	let is_some = function
		None -> false
	|	_ -> true

	let get_some = function
		Some x -> x
	|	_ -> raise Not_found
	
	let map_some opt f =
		match opt with
			Some x -> Some (f x)
		|	None -> None
		
	let resolve opt f n =
		match opt with
			Some x -> f x
		|	None -> n
		
end;;


module Tuple = struct

	type ('a, 'b) t = 'a * 'b
	type 'a s = ('a, 'a) t
	
	let assemble x y = (x, y)
	
	let fst (x, _) = x
	
	let snd (_, y) = y
	
	let get (x, y) i =
		if i = 0
		then x
		else y

end;;


module Triple = struct

	type ('a, 'b, 'c) t = 'a * 'b * 'c
	type 'a s = ('a, 'a, 'a) t
	
	let assemble x y z = (x, y, z)
	
	let fst (x, _, _) = x
	
	let snd (_, y, _) = y
	
	let trd (_, _, z) = z
	
	let get (x, y, z) i =
		if i = 0
		then x
		else if i = 1
		then y
		else z

end;;



module Comparators = struct

	type 'a comparator = ('a -> 'a -> int)
	
	let default = compare
	
	let nocompare _ _ = 0
	
	let product f1 f2 (x1,x2) (y1,y2) =
		let c = f1 x1 y1 in
		if c = 0 then f2 x2 y2 else c

	let product3 f1 f2 f3 (x1,x2,x3) (y1,y2,y3) =
		let c = f1 x1 y1 in
		if c != 0 then c
		else let c = f2 x2 y2 in
		if c != 0 then c
		else f3 x3 y3

	let product4 f1 f2 f3 f4 (x1,x2,x3,x4) (y1,y2,y3,y4) =
		let c = f1 x1 y1 in
		if c != 0 then c
		else let c = f2 x2 y2 in
		if c != 0 then c
		else let c = f3 x3 y3 in
		if c != 0 then c
		else f4 x4 y4

	let product5 f1 f2 f3 f4 f5 (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) =
		let c = f1 x1 y1 in
		if c != 0 then c
		else let c = f2 x2 y2 in
		if c != 0 then c
		else let c = f3 x3 y3 in
		if c != 0 then c
		else let c = f4 x4 y4 in
		if c != 0 then c
		else f5 x5 y5

	let inner_product f1 f2 x y =
		let c = f1 x y in
		if c = 0 then f2 x y else c
		
	let option_compare f x y =
		match (x,y) with
			(None, None) -> 0
		|	(None, _) -> -1
		|	(_, None) -> 1
		|	(Some xx, Some yy) -> f xx yy
	
	let by_map f m x y =
		f (m x) (m y)
		
	let by_default_map m x y =
		compare (m x) (m y)
	
end;;


module Formators = struct

	type 'a formator = ('a -> string)
	
	
	let def_int = string_of_int
	
	let def_string s = s
	
	let def_bool = string_of_bool
		
	let product f1 f2 (x1,x2) =
		"(" ^ f1 x1 ^ "," ^ f2 x2 ^ ")"
		
	let product3 f1 f2 f3 (x1,x2,x3) =
		"(" ^ f1 x1 ^ "," ^ f2 x2 ^ "," ^ f3 x3 ^ ")"

	let option_format none some f = function
		None -> none
	|	Some x -> some ^ "(" ^ f x ^ ")"

end;;


module Iterators = struct

	type 'a iterator = ('a -> bool) -> unit
	
    type 'a full_iterator = ('a -> unit) -> unit

	let is_empty iterator =
		let empty = ref true in
		iterator (fun _ -> empty := false; false);
		!empty
		
	let cardinal iterator =
		let i = ref 0 in
		iterator (fun _ ->
			incr i;
			true
		);
		!i	
		
	let first iterator =
		let x = ref None in
		iterator (fun y ->
			x := Some y;
			false
		);
		OptionUtils.get_some !x

	let iter iterator f =
		iterator (fun x ->
			f x;
			true
		)
		
	let map mp iterator f =
		iterator (fun x -> f (mp x))
		
	let exists iterator f =
		let result = ref false in
		iterator (fun x ->
			result := f x;
			not !result
		);
		!result
	
	let forall iterator f =
		let result = ref true in
		iterator (fun x ->
			result := f x;
			!result
		);
		!result
		
	let fold iterator f init =
		let a = ref init in
		iterator (fun x ->
			a := f x !a;
			true
		);
		!a
		
	let filter filt iterator =
		(fun f ->
			iterator (fun x ->
				(not (filt x)) || (f x)
			)
		)
		
	let to_list iterator =
		let l = ref [] in
		iterator (fun x ->
			l := x::!l;
			true
		);
		List.rev !l
		
	let rec of_list l f =
		match l with
			[] -> ()
		|	x::xs -> if f x then of_list xs f
		
	let explicit iterator =
		of_list (to_list iterator)
		
	let singleton x f =
		let _ = f x in ()
		
	let to_array iterator =
		Array.of_list (to_list iterator)
		
	let of_array a f =
		let n = Array.length a in
		let i = ref 0 in
		while (!i < n) && (f a.(!i)) do
			incr i
		done
		
	let of_full_iterator iter f =
		let res = ref true in
		iter (fun x -> if !res then res := f x)

	let second_if_first_empty it1 it2 f =
		let empty = ref true in
		it1 (fun x ->
			empty := false;
			f x
		);
		if !empty then it2 f
		
	let attach it1 it2 f =
		let result = ref true in
		it1 (fun x ->
			result := f x;
			!result
		);
		if !result then it2 f
		
	let flatten iterator_iterator f =
		iterator_iterator (fun sub_iterator ->
			let result = ref true in
			sub_iterator (fun x ->
				result := f x;
				!result
			);
			!result
		)		
		
	let product it1 it2 =
		(fun f ->
			let result = ref true in
			it1 (fun x ->
				if !result then (
					it2 (fun y ->
						result := f (x,y);
						!result
					)
				);
				!result
			)
		)
		
	let depend_product it1 it2 =
		(fun f ->
			let result = ref true in
			it1 (fun x ->
				if !result then (
					it2 x (fun y ->
						result := f (x,y);
						!result
					)
				);
				!result
			)
		)

	let mapped_product it1 it2 =
		(fun f ->
			let result = ref true in
			it1 (fun x ->
				if !result then (
					it2 x (fun y ->
						result := f y;
						!result
					)
				);
				!result
			)
		)
		
	let option_iterator it =
		(fun f -> if f None then (map (fun x -> Some x) it) f else ())

end;;


module Enumerators = struct

	type 'a enumerator = Empty | Next of ('a enumerator' * 'a)
	and 'a enumerator' = unit -> 'a enumerator

	type ('a, 'b) t = 'b -> ('b * 'a) option
	
	let make factory init =
		let rec f state _ = 
			match factory state with
				None -> Empty
			|	Some (state', x) -> Next (f state', x)
		in
			f init ()
			
	let empty = function
		Empty -> true
	|	_ -> false
	
	let head = function
		Empty -> raise Not_found
	|	Next (_, x) -> x
	
	let tail = function
		Empty -> raise Not_found
	|	Next (y, _) -> y ()
	
	let next = function
		Empty -> raise Not_found
	|	Next (y, x) -> (y (), x)

	let to_iterator enum f =
		let rec g = function
			Empty -> ()
		|	Next (y, x) -> if f x then g (y ()) else ()
		in
			g enum
	
	let of_array arr =
		let l = Array.length arr in
		let f i = 
			if i >= l then None
			else Some (i+1, arr.(i))
		in
			make f 0
			
	let of_list li =
		let f = function
			[] -> None
		|	(x::xs) -> Some (xs,x)
		in
			make f li
			
	let singleton x =
		make (fun state -> OptionUtils.map_some state (fun y -> (None, y))) (Some x)
			
	let rec map f = function
		Empty -> Empty
	|	Next (y, x) -> Next ((fun un -> map f (y un)), f x)
	
	let to_list e = Iterators.to_list (to_iterator e)
end;;


module Domain = struct

	type 'a t = ('a Comparators.comparator) * ('a Formators.formator)
	
	let make cmp fmt = (cmp, fmt)

	let compare = fst
	
	let format = snd
	
	let product (cmp1, fmt1) (cmp2, fmt2) =
		make (Comparators.product cmp1 cmp2)
		     (Formators.product fmt1 fmt2)

	let option_domain (cmp, fmt) =
		make (Comparators.option_compare cmp) (Formators.option_format "None" "Some" fmt)
		      
end;;
	

module UniqueIntegerAssignment = struct

    type t = (int ref) * ((int list) ref)
	
	let new_assignment _ =
		(ref 0, ref [])
		
	let assign (limit, lst) =
		match !lst with
			[] -> (
				incr limit;
				!limit - 1
			)
		|	x::xs -> (
				lst := xs;
				x
			)

	let release (limit, lst) i =
		if i = !limit - 1
		then decr limit
		else lst := i::!lst

end;;
  
  

module CompRef = struct
	
	type 'a compref =  {
		index: int;
		mutable content : 'a;
	}

	let uia =
		UniqueIntegerAssignment.new_assignment ()
	
	let newref x = {
		index = UniqueIntegerAssignment.assign uia;
		content = x
	}
		
	let freeref x =
		UniqueIntegerAssignment.release uia x.index
		
	let getref x =
		x.content
		
	let setref x y =
		x.content <- y
		
	let compare a b =
		compare a.index b.index
		
	let equal a b =
		a == b
		
end;;



module Bits = struct
	
	type t = int array
	
	let make n i = Array.make n i
	
	let zero n = make n 0
	
	let one n = make n 1
	
	let least bits b =
		let n = Array.length bits in
		let rec helper i =
			if bits.(i) = b
			then i
			else if i < n
			then helper (i+1)
			else if b = 0
			then n
			else -1
		in
  	helper 0 
		
	let least_zero bits = least bits 0
	
	let least_one bits = least bits 1
	
	let inc bits =
		let z = least_zero bits in
		Array.init (Array.length bits) (fun i ->
			if i <= z then 1 - bits.(i) else bits.(i)
		)
		
	let shl bits =
		Array.init (Array.length bits) (fun i ->
			if i = 0 then 0 else bits.(i - 1)
		)
		
	let shr bits =
		let n = Array.length bits in
		Array.init (Array.length bits) (fun i ->
			if i = n - 1 then 0 else bits.(i + 1)
		)
		
	let to_int bits =
		Array.fold_right (
			fun b acc -> 2 * acc + b
		) bits 0
		
	let of_int i =
		let rec h i =
			if i = 0
			then []
			else (i mod 2)::h (i/2)
		in
		Array.of_list (h i)
	
end;;
