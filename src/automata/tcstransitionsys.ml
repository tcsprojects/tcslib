open Tcstiming;;
open Tcsarray;;
open Tcsset;;


type ('s, 'l, 'p) initlts =
	('s *
	 ('s -> 'p list) *
	 ('s -> ('l * 's) list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('l -> string) *
	 ('p -> string))

let get_timed_initlts ((start, props, delta, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let props_timed s =
		SimpleTiming.start timingobj;
		let b = props s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, props_timed, delta_timed, fmt), a)

let get_cached_initlts ((start, props, delta, fmt), (cmp_state', fmt_lbl, fmt_prp)) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initlts: Compare function required."
	in
	let data_cache = ref (TreeMap.empty cmp_state) in
	let touch x =
		try
			TreeMap.find x !data_cache
		with Not_found -> (
			let y = (props x, ref None) in
			data_cache := TreeMap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun (_, x) -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let props_cached x = let (o, _) = touch x in o in
	((start, props_cached, delta_cached, fmt), (cmp_state', fmt_lbl, fmt_prp))

let get_int_cached_initlts ((start, props, delta, fmt), (cmp_state', fmt_lbl, fmt_prp))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initlts: Compare function required."
	in
	let state_to_int = ref (TreeMap.empty cmp_state) in
	let int_to_state = DynArray.create (start, [], ref None) in
	let state_to_int' s =
		try
			TreeMap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := TreeMap.add s i !state_to_int;
			DynArray.add int_to_state (s, props s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _) = DynArray.get int_to_state i in s in
	let props' i = let (_, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = List.map (fun (l, x) -> (l, state_to_int' x)) (delta s) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, props', delta', fmt'),
	  (Some compare, fmt_lbl, fmt_prp)),
	 state_to_int',
	 int_to_state')

type explicit_lts =
	string array * (* propositions *)
	string array * (* labels *)
	(int array * (* propositions *)
	 (int * int) array * (* label,node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initlts = int * explicit_lts

let build_explicit_initlts (((start, props, delta, _), (_, fmt_lbl, fmt_prp)): (int, 'l, 'p) initlts)
                           (int_to_state: (int -> 's))
						   (fmt: ('s -> string option))
						   (current_state_event: (int -> unit)) =
	let prop_to_int = ref (TreeMap.empty compare) in
	let prop_count = ref 0 in
	let label_to_int = ref (TreeMap.empty compare) in
	let label_count = ref 0 in
	let map_prop p =
		try
			TreeMap.find p !prop_to_int
		with Not_found -> (
			let i = !prop_count in
			prop_to_int := TreeMap.add p i !prop_to_int;
			incr prop_count;
			i
		)
	in
	let map_label p =
		try
			TreeMap.find p !label_to_int
		with Not_found -> (
			let i = !label_count in
			label_to_int := TreeMap.add p i !label_to_int;
			incr label_count;
			i
		)
	in
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun (_, i) -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let graph = Array.init (!max_int + 1) (fun i ->
		(Array.of_list (List.map map_prop (props i)),
		 Array.of_list (List.map (fun (l,j) -> (map_label l, j)) (delta i)),
		 fmt (int_to_state i),
		 true)
	) in
	let proparr = Array.make !prop_count "" in
	TreeMap.iter (fun s i -> proparr.(i) <- fmt_prp s) !prop_to_int;
	let labelarr = Array.make !label_count "" in
	TreeMap.iter (fun s i -> labelarr.(i) <- fmt_lbl s) !label_to_int;
	(start, (proparr, labelarr, graph))

let print_explicit_lts_plain (props, labels, data) printer =
	let n = Array.length data in
	for i = 0 to n - 1 do
		let (nodeprops, nodetrans, nodeann, valid) = data.(i) in
		if valid then (
			printer (string_of_int i ^ " ");
			printer (String.concat "," (List.map (fun (l,j) -> labels.(l) ^ ":" ^ string_of_int j) (Array.to_list nodetrans)));
			printer " ";
			printer (String.concat "," (List.map (fun j -> props.(j)) (Array.to_list nodeprops)));
            (match nodeann with
				None -> ()
            |   Some s -> if s <> "" then printer (" \"" ^ s ^ "\""));
            printer ";\n"
		)
	done

let print_explicit_lts (props, labels, data) printer =
	printer ("lts " ^ string_of_int (Array.length data-1) ^ ";\n");
	print_explicit_lts_plain (props, labels, data) printer

let print_explicit_initlts (init, (props, labels, data)) printer =
	printer ("lts " ^ string_of_int (Array.length data-1) ^ ";\n");
	printer ("start " ^ string_of_int init ^ ";\n");
	print_explicit_lts_plain (props, labels, data) printer

type ('s, 'p) initts =
	('s *
	 ('s -> 'p list) *
	 ('s -> 's list) *
	 ('s -> string)) *
	(('s -> 's -> int) option *
	 ('p -> string))

let get_timed_initts ((start, props, delta, fmt), a) timingobj =
	let delta_timed s =
		SimpleTiming.start timingobj;
		let l = delta s in
		SimpleTiming.stop timingobj;
		l
	in
	let props_timed s =
		SimpleTiming.start timingobj;
		let b = props s in
		SimpleTiming.stop timingobj;
		b
	in
	((start, props_timed, delta_timed, fmt), a)

let get_cached_initts ((start, props, delta, fmt), (cmp_state', fmt_prp)) =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_cached_initlts: Compare function required."
	in
	let data_cache = ref (TreeMap.empty cmp_state) in
	let touch x =
		try
			TreeMap.find x !data_cache
		with Not_found -> (
			let y = (props x, ref None) in
			data_cache := TreeMap.add x y !data_cache;
			y
		)
	in
	let delta_cached x =
		let (_, d) = touch x in
		match !d with
			Some l -> l
		|	None -> (
			let l = delta x in
			List.iter (fun x -> let _ = touch x in ()) l;
			d := Some l;
			l
		)
	in
	let props_cached x = let (o, _) = touch x in o in
	((start, props_cached, delta_cached, fmt), (cmp_state', fmt_prp))

let get_int_cached_initts ((start, props, delta, fmt), (cmp_state', fmt_prp))
                          new_state_event =
	let cmp_state = match cmp_state' with
		Some s -> s
	|	_ -> failwith "get_int_cached_initlts: Compare function required."
	in
	let state_to_int = ref (TreeMap.empty cmp_state) in
	let int_to_state = DynArray.create (start, [], ref None) in
	let state_to_int' s =
		try
			TreeMap.find s !state_to_int
		with Not_found -> (
			let i = DynArray.length int_to_state in
			state_to_int := TreeMap.add s i !state_to_int;
			DynArray.add int_to_state (s, props s, ref None);
			new_state_event s i;
			i
		)
	in
	let int_to_state' i = let (s, _, _) = DynArray.get int_to_state i in s in
	let props' i = let (_, s, _) = DynArray.get int_to_state i in s in
	let fmt' i = fmt (int_to_state' i) in
	let delta' i =
		let (s, _, tr) = DynArray.get int_to_state i in
		match !tr with
			Some l -> l
		|	None -> (
				let l = List.map (fun x -> state_to_int' x) (delta s) in
				tr := Some l;
				l
			)
	in
	(((state_to_int' start, props', delta', fmt'),
	  (Some compare, fmt_prp)),
	 state_to_int',
	 int_to_state')

type explicit_ts =
	string array * (* propositions *)
	(int array * (* propositions *)
	 int array * (* node transitions *)
	 string option * (* annotation *)
	 bool (* valid *)
	) array

type explicit_initts = int * explicit_ts

let build_explicit_initts (((start, props, delta, _), (_, fmt_prp)): (int, 'p) initts)
                           (int_to_state: (int -> 's))
						   (fmt: ('s -> string option))
						   (current_state_event: (int -> unit)) =
	let prop_to_int = ref (TreeMap.empty compare) in
	let prop_count = ref 0 in
	let map_prop p =
		try
			TreeMap.find p !prop_to_int
		with Not_found -> (
			let i = !prop_count in
			prop_to_int := TreeMap.add p i !prop_to_int;
			incr prop_count;
			i
		)
	in
	let max_int = ref 0 in
	let cur_int = ref 0 in
	while !cur_int <= !max_int do
		current_state_event !cur_int;
		List.iter (fun i -> max_int := max !max_int i) (delta !cur_int);
		incr cur_int
	done;
	let graph = Array.init (!max_int + 1) (fun i ->
		(Array.of_list (List.map map_prop (props i)),
		 Array.of_list (delta i),
		 fmt (int_to_state i),
		 true)
	) in
	let proparr = Array.make !prop_count "" in
	TreeMap.iter (fun s i -> proparr.(i) <- fmt_prp s) !prop_to_int;
	(start, (proparr, graph))

let print_explicit_ts_plain (props, data) printer =
	let n = Array.length data in
	for i = 0 to n - 1 do
		let (nodeprops, nodetrans, nodeann, valid) = data.(i) in
		if valid then (
			printer (string_of_int i ^ " ");
			printer (String.concat "," (List.map string_of_int (Array.to_list nodetrans)));
			printer " ";
			printer (String.concat "," (List.map (fun j -> props.(j)) (Array.to_list nodeprops)));
            (match nodeann with
				None -> ()
            |   Some s -> if s <> "" then printer (" \"" ^ s ^ "\""));
            printer ";\n"
		)
	done

let print_explicit_ts (props, data) printer =
	printer ("ts " ^ string_of_int (Array.length data-1) ^ ";\n");
	print_explicit_ts_plain (props, data) printer

let print_explicit_initts (init, (props, data)) printer =
	printer ("ts " ^ string_of_int (Array.length data-1) ^ ";\n");
	printer ("start " ^ string_of_int init ^ ";\n");
	print_explicit_ts_plain (props, data) printer
