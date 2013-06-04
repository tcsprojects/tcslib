module ListUtils = struct

	let custom_format formater left right del = function
		[] -> left ^ right
	|	(h::t) -> (List.fold_left (fun s i -> s ^ del ^ (formater i)) (left ^ (formater h)) t) ^ right

	let format formater = custom_format formater "[" "]" ", "
	
	let format_plain formater = custom_format formater "" "" " "
	
	let init c f =
		let rec helper acc i =
			if i < 0 then acc
			else helper ((f i)::acc) (i - 1)
		in
			helper [] (c - 1)
	
	let make c a = init c (fun _ -> a)
	
	let compare_lists cmp l1 l2 =
		let rec comp = function
			(x::xs, y::ys) -> let c = cmp x y in
			                  if c != 0 then c
			                  else comp (xs, ys)
		|	_ -> 0
		in
		let c = compare (List.length l1) (List.length l2) in
		if c != 0 then c else comp (l1, l2)

	let rec intersperse y = function
		[] -> []
	|	[x] -> [x]
	|	x::xs -> x::y::(intersperse y xs)
	
	let min_elt cmp l =
		let rec helper m = function
			[] -> m
		|	x::xs -> helper (if cmp x m >= 0 then m else x) xs
		in
		helper (List.hd l) (List.tl l)
		
	let max_elt cmp = min_elt (fun x y -> cmp y x)

	let iteri f =
		let i = ref 0 in
		List.iter (fun e ->
			f !i e;
			incr i
		)
		
	let filter_map f l =
		List.fold_right (fun x m -> 
			match f x with
				Some y -> y::m
			|	None -> m
		) l [] 
	
end;;


module IntListUtils = struct

	let custom_format = ListUtils.custom_format string_of_int
	
	let format = ListUtils.format string_of_int
	
	let format_plain = ListUtils.format_plain string_of_int

end;;

