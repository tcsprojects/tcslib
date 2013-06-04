open Tcstiming;;
open Tcsarray;;
open Tcsset;;
open Tcsbasedata;;
open Tcslist;;


type 's initpg = ('s *
                  ('s -> 's list) *
                  ('s -> int) *
                  ('s -> bool) *
                  ('s -> string)) *
                 (int option *
                  int option *
                  ('s -> 's -> int) option)

type 's initpg_solution = 's -> bool option

type 's initpg_strategy = 's -> 's option				  
		
let get_compact_initpg (((start, delta, omega, player, fmt), (_, index, cmp)): 's initpg) identify_state =
	let start' = (start, 0) in
	let omega' (_, p) = p in
	let player' (q, _) = player q in
	let fmt' (q, p) = fmt q ^ " " ^ string_of_int p in
	let size' = None in
	let index' = index in
	let cmp' = match cmp with
		Some c -> Some (fun (x, px) (y, py) ->
							let res = c x y in
							if res = 0 then compare px py else res)
	|	None -> None
	in
	
	let compare' =
		match cmp' with
			Some c -> c
		|	None -> compare
	in
	
	let delta_default s =
		List.map (fun s' -> (s', omega s')) (delta s)
	in
	
	let delta_acc (s, p) =
		List.map (fun s' -> (s', max p (omega s'))) (delta s)
	in
	
	let delta' (s: 's * int) =
		let helper pl init =
			let states = ref (TreeSet.empty compare') in
			let todo = ref (TreeSet.singleton compare' s) in
			while (not (TreeSet.is_empty !todo)) do
				let s = TreeSet.min_elt !todo in
				todo := TreeSet.remove s !todo;
				if not (TreeSet.mem s !states) then (
					if (player' s = pl) && (not (identify_state (fst s)))
					then List.iter (fun s' -> todo := TreeSet.add s' !todo) (delta_acc s)
					else states := TreeSet.add s !states
				)
			done;
			TreeSet.elements !states
		in
			if identify_state (fst s) then delta_default (fst s) else helper (player' s) s
	in
	
	((start', delta', omega', player', fmt'), (size', index', cmp'));;

let get_compact_initpg_by_player (pg: 's initpg) by_player =
	let ((_, _, _, pl, _), _) = pg in
	let plf s = pl s != by_player in
	get_compact_initpg pg plf;;
	
let get_escaped_initpg (((start, delta, omega, player, fmt), (size, index, cmp)): 's initpg) x =

	(((start, x),
	  (fun (s, y) -> List.map (fun s' -> (s', y)) (delta s)),
	  (fun (s, _) -> omega s),
	  (fun (s, _) -> player s),
	  (fun (s, _) -> fmt s)),
	 ((size,
	   index,
	   (match cmp with Some c -> Some (fun a b -> c (fst a) (fst b)) | None -> None))));;

				  
let get_timed_initpg ((start, delta, omega, player, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let omega_timed s =
		SimpleTiming.start timingobj;
		let b = omega s in
		SimpleTiming.stop timingobj;
		b
	in
	let player_timed s =
		SimpleTiming.start timingobj;
		let b = player s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, delta_timed, omega_timed, player_timed, fmt), a)

let get_cached_initpg ((start, delta, omega, player, fmt), (size, index, cmp_state')) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initpg: Compare function required."
	in
	let data_cache = ref (TreeMap.empty cmp_state) in
	let touch x =
		try
			TreeMap.find x !data_cache
		with Not_found -> (
			let y = (omega x, player x, ref None) in
			data_cache := TreeMap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, _, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun x -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let omega_cached x = let (o, _, _) = touch x in o in
	let player_cached x = let (_, p, _) = touch x in p in
	((start, delta_cached, omega_cached, player_cached, fmt), (size, index, cmp_state'))

let get_int_cached_initpg ((start, delta, omega, player, fmt), (size, index, cmp_state'))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initpg: Compare function required."
	in
	let state_to_int = ref (TreeMap.empty cmp_state) in
	let int_to_state = DynArray.create (start, -1, false, ref None) in
	let state_to_int' s =
		try
			TreeMap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := TreeMap.add s i !state_to_int;
			DynArray.add int_to_state (s, omega s, player s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _, _) = DynArray.get int_to_state i in s in
	let omega' i = let (_, s, _, _) = DynArray.get int_to_state i in s in
	let player' i = let (_, _, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = TreeSet.elements (TreeSet.of_list_def (List.map state_to_int' (delta s))) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, delta', omega', player', fmt'),
	  (size, index, Some compare)),
	 state_to_int',
	 int_to_state')


type explicit_pg = (int * int * int array * string) array

type explicit_initpg = int * explicit_pg

let build_explicit_initpg (((start, delta, omega, player, fmt), _): int initpg) current_state_event =
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun i -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let pg = (Array.init (!max_int + 1) (fun i ->
		(omega i, (if player i then 0 else 1), Array.of_list (delta i), fmt i)
	): explicit_pg) in
	(start, pg)

let print_explicit_pg_plain game printer =
	Array.iteri (fun i (pr, pl, delta, desc) ->
		if pr >= 0 && pl >= 0 && pl <= 1 then (
			printer (string_of_int i ^ " " ^ string_of_int pr ^ " " ^ string_of_int pl ^ " ");
			printer (String.concat "," (Array.to_list (Array.map string_of_int delta)));
            if desc <> "" then printer (" \"" ^ desc ^ "\"");
            printer ";\n"
        )
	) game

let print_explicit_pg pg printer =
	printer ("parity " ^ string_of_int (Array.length pg-1) ^ ";\n");
	print_explicit_pg_plain pg printer

let print_explicit_initpg (i, pg) printer =
	printer ("parity " ^ string_of_int (Array.length pg-1) ^ ";\n");
	printer ("start " ^ string_of_int i ^ ";\n");
	print_explicit_pg_plain pg printer

type explicit_pg_solution = int array

type explicit_pg_strategy = int array

let print_explicit_pg_solution_strategy sol strat printer =
	let n = Array.length sol in
	printer ("paritysol " ^ string_of_int (n-1) ^ ";\n");
	for i = 0 to n - 1 do
		if sol.(i) >= 0 then (
            printer (string_of_int i ^ " " ^ string_of_int sol.(i));
            if strat.(i) >= 0
			then printer (" " ^ string_of_int strat.(i));
			printer ";\n"
        )
    done

