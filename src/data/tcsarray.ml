open Tcsbasedata;;

module ArrayUtils = struct

	let of_rev_list l =
		let n = List.length l in
		if n = 0 then [||]
		else let a = Array.make n (List.hd l) in
			 let i = ref n in
			 List.iter (fun e ->
				decr i;
				a.(!i) <- e
			) l;
			a

	let sort_with_permutations arr compare =
		let n = Array.length arr in
		if n = 0 then ([||], (fun i -> i), (fun i -> i)) else (
			let newToOld = Array.init n (fun i -> i) in
			Array.sort (fun i j -> compare arr.(i) arr.(j)) newToOld;
			let oldToNew = Array.make n (-1) in
			let returnarr = Array.make n arr.(0) in
			for i = 0 to n - 1 do
				oldToNew.(newToOld.(i)) <- i;
				returnarr.(i) <- arr.(newToOld.(i))
			done;
			(returnarr, (fun i -> if i < 0 || i >= n then i else newToOld.(i)), (fun i -> if i < 0 || i >= n then i else oldToNew.(i)))
		);;
		
	let mem element arr =
		let n = Array.length arr in
		let i = ref 0 in
		while (!i < n) && (not (arr.(!i) = element)) do
			incr i
		done;
		!i < n;;

	let forall arr pred =
		let res = ref true in
		let i = ref 0 in
		let n = Array.length arr in
		while !res && (!i < n) do
			res := pred !i arr.(!i);
			incr i
		done;
		!res

	let exists arr pred =
		let res = ref false in
		let i = ref 0 in
		let n = Array.length arr in
		while (not !res) && (!i < n) do
			res := pred !i arr.(!i);
			incr i
		done;
		!res

	let filter f a =
		Array.of_list (List.filter f (Array.to_list a))

	let custom_format formater left right del arr =
		if Array.length arr = 0 then left ^ right
		else let fst = ref true in
		     Array.fold_left (fun s i ->
		        let t = if !fst then formater i else del ^ (formater i) in
		     	fst := false;
		     	s ^ t
		     ) left arr ^ right

	let format formater = custom_format formater "[" "]" ", "
	
	let format_plain formater = custom_format formater "" "" " "
	
	let custom_formati formater left right del arr =
		if Array.length arr = 0 then left ^ right
		else let fst = ref true in
			 let j = ref 0 in
		     Array.fold_left (fun s i ->
		        let t = if !fst then formater !j i else del ^ (formater !j i) in
		        incr j;
		     	fst := false;
		     	s ^ t
		     ) left arr ^ right

	let formati formater = custom_formati formater "[" "]" ", "
	
	let format_plaini formater = custom_formati formater "" "" " "

	let find2 p a i =
		let i = ref i in
		let n = Array.length a in
		while (!i < n) && (not (p a.(!i))) do
			incr i
		done;
		if !i < n then !i else raise Not_found
		
	
	let find p a = find2 p a 0
	
	let index_of2 a x i = find2 (fun y -> x = y) a i
	
	let index_of a x = index_of2 a x 0


	let find2_last p a i =
		let i = ref i in
		while (!i >= 0) && (not (p a.(!i))) do
			decr i
		done;
		if !i >= 0 then !i else raise Not_found


	let find_last p a = find2_last p a (Array.length a - 1)
	
	let index_of2_last a x i = find2_last (fun y -> x = y) a i
	
	let index_of_last a x = index_of2_last a x (Array.length a - 1)

	let min_elt_idx cmp a =
		let m = ref 0 in
		let n = Array.length a in
		for i = 1 to n - 1 do
			if cmp a.(i) a.(!m) < 0 then m := i
		done;
		!m

	let max_elt_idx cmp = min_elt_idx (fun x y -> cmp y x)

	let min_elt cmp a = a.(min_elt_idx cmp a)

	let max_elt cmp = min_elt (fun x y -> cmp y x)
	
	let shuffle arr =
		let n = Array.length arr in
		let idx = Array.init n (fun i -> i) in
		Array.init n (fun i ->
			let j = Random.int (n-i) in
			let x = arr.(idx.(j)) in
			idx.(j) <- idx.(n-i-1);
			x
		)
	
end;;


module IntArrayUtils = struct

	let custom_format = ArrayUtils.custom_format string_of_int
	
	let format = ArrayUtils.format string_of_int
	
	let format_plain = ArrayUtils.format_plain string_of_int

end;;


module DynArray = struct
	 type 'a t = Intern of (int ref) * 'a * ((int -> int -> int) ref) * ('a array ref)

	 type capacity_updater = int -> int -> int

	 let update_capacity upd arr len def =
		let capacity = Array.length !arr in
		let newCapacity = upd len capacity in
			if capacity != newCapacity then
				let arr' = Array.make newCapacity def in (
					for i = 0 to (min len capacity) - 1 do
						arr'.(i) <- !arr.(i)
					done;
					arr := arr'
			)

	 let default_capacity_updater len old_capacity =
		if (old_capacity < len) || (len * 3 < old_capacity)
		then len * 2
		else old_capacity

	 let get_capacity_updater (Intern (_, _, upd, _)) = !upd

	 let set_capacity_updater (Intern (_, _, upd, _)) upd' =
		upd := upd'

	 let create def = Intern (ref 0, def, ref default_capacity_updater, ref (Array.make 0 def))

	 let init def n f = 
		let a = Array.make n def in
			for i = 0 to n - 1 do
				a.(i) <- f i
			done;
			Intern (ref n, def, ref default_capacity_updater, ref a)
			
	 let copy (Intern (len, def, upd, arr)) =
		Intern (ref !len, def, ref !upd, ref (Array.copy !arr))
	 
	 let clear (Intern (len, def, _, arr)) = (
		len := 0;
		arr := Array.make 0 def
	 )

	 let length (Intern (len, _, _, _)) = !len

	 let insert (Intern (len, def, upd, arr)) idx el = (
		if (idx < 0) || (idx > !len)
		then invalid_arg "DynArray.insert: index out of bounds";
		update_capacity !upd arr (!len + 1) def;
		for i = !len - 1 downto idx do
			!arr.(i + 1) <- !arr.(i)
		done;
		len := !len + 1;
		!arr.(idx) <- el
	 )
	 
	 let fill (Intern (len, def, upd, arr)) upto fillwith =
		let len' = max !len upto in 	
		update_capacity !upd arr (len') def;
		for i = !len to len' - 1 do
			!arr.(i) <- fillwith
		done;
		len := len'
	 
	 let append (Intern (len', _, _, arr')) (Intern (len, def, upd, arr)) =
		update_capacity !upd arr (!len + !len') def;
		for i = 0 to !len' - 1 do
			!arr.(!len + i) <- !arr'.(i)
		done;
		len := !len + !len'

	 let delete (Intern (len, def, upd, arr)) idx = (
		if (idx < 0) || (idx >= !len)
		then invalid_arg "DynArray.delete: index out of bounds";
		for i = idx + 1 to !len - 1 do
			!arr.(i - 1) <- !arr.(i)
		done;
		update_capacity !upd arr (!len - 1) def;
		len := !len - 1
	 )

	 let delete_last (Intern (len, def, upd, arr)) = (
		update_capacity !upd arr (!len - 1) def;
		len := !len - 1
	 )

	 let get (Intern (len, _, _, arr)) idx = (
		if (idx < 0) || (idx >= !len)
		then invalid_arg "DynArray.get: index out of bounds";
		!arr.(idx)
	 )

	 let set (Intern (len, _, _, arr)) idx value = (
		if (idx < 0) || (idx >= !len)
		then invalid_arg "DynArray.get: index out of bounds";
		!arr.(idx) <- value
	 )

	 let add (Intern (len, def, upd, arr)) el =
		insert (Intern (len, def, upd, arr)) !len el

	 let iter (f: 'a -> unit) (Intern (len, _, _, arr)) =
		for i = 0 to !len - 1 do
			f !arr.(i)
		done

	 let iteri (f: int -> 'a -> unit) (Intern (len, _, _, arr)) =
		for i = 0 to !len - 1 do
			f i !arr.(i)
		done

 	 let to_array (Intern (len, def, _, arr)) =
		let a = Array.make !len def in
			for i = 0 to !len - 1 do
				a.(i) <- !arr.(i)
			done;
		a

         let to_list (Intern (len, def, _, arr)) =
                let l = ref [] in
                for i = !len - 1 downto 0 do
                   l := !arr.(i) :: !l
                done;
                !l

	 let map def f (Intern (len, _, upd, arr)) =
		let a = Array.make !len def in
			for i = 0 to !len - 1 do
				a.(i) <- f (!arr.(i))
			done;
			Intern (ref (!len), def, upd, ref a)
			
	 let map_inplace f (Intern (len, _, upd, arr)) =
		for i = 0 to !len - 1 do
			!arr.(i) <- f (!arr.(i))
		done

	 let fold_left f init intern =
		let v = ref init in
			iter (fun x -> v := f !v x) intern;
			!v
			
	 let push = add
	 
	 let pop arr =
		let last = length arr - 1 in
		let x = get arr last in
		delete arr last;
		x
		
	 let delete_disregarding_ordering arr i =
		let last = length arr - 1 in
		set arr i (get arr last);
		delete arr last
		
	 let take_random arr =
		let i = Random.int (length arr) in
		let x = get arr i in
		delete_disregarding_ordering arr i;
		x
	
	let to_iterator arr f =
		let n = length arr in
		let i = ref 0 in
		while (!i < n) && (f (get arr !i)) do
			incr i
		done
	
	let to_iteratori arr f =
		let n = length arr in
		let i = ref 0 in
		while (!i < n) && (f (!i, get arr !i)) do
			incr i
		done

	let append_iterator arr it =
		Iterators.iter it (add arr)
		
end;;
