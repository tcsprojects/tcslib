open Tcstransitionsysparserinternal;;
open Tcsset;;

exception TransitionSysParserException of string * int * int

exception TransitionSysParserCustomException of string

let parse_lts has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__lts_has_header := __def_lts_has_header;
		__lts_has_start := __def_lts_has_start;
		__lts_add_node := __def_lts_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (TransitionSysParserException (s, l, c))
	);
	__lts_has_header := has_header;
	__lts_has_start := has_start;
	__lts_add_node := add_func;
	(try
		Tcsltsparser.lts Tcsltslexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	TransitionSysParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	
let parse_explicit_lts' in_channel init_required =
	let init_value = ref (-1) in
	let lts = ref [||] in
	let props = ref TreeMap.empty_def in
	let props_count = ref 0 in
	let labels = ref TreeMap.empty_def in
	let labels_count = ref 0 in
	let register_tr (s,j) =
		try
			(TreeMap.find s !labels, j)
		with
			Not_found -> (
				labels := TreeMap.add s !labels_count !labels;
				incr labels_count;
				(!labels_count - 1, j)
			)
	in
	let register_pr s =
		try
			TreeMap.find s !props
		with
			Not_found -> (
				props := TreeMap.add s !props_count !props;
				incr props_count;
				!props_count - 1
			)
	in
	let map_to_array map count = 
		let a = Array.make count "" in
		TreeMap.iter (fun s i -> a.(i) <- s) map;
		a
	in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i tr pr de =
		if (i < 0) || (i >= Array.length !lts)
		then raise (TransitionSysParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if let (_, _, _, v) = !lts.(i) in v
		then raise (TransitionSysParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!lts.(i) <- (pr, tr, de, true)
	in
	let add_func_old i tr pr de =
		node_list := (i, tr, pr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_lts _ = (map_to_array !props !props_count, map_to_array !labels !labels_count, !lts) in
	let ret_func_old _ = (
		lts := Array.make (!node_list_max + 1) ([||], [||], None, false);
		List.iter (fun (a,b,c,d) -> add_value a b c d) !node_list;
		ret_lts ()
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		lts := Array.make (n + 1) ([||], [||], None, false);
		add_func := add_value;
		ret_func := ret_lts
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (TransitionSysParserCustomException "Init node out of range.")
	)
	in
	let do_add i tr pr de = !add_func i (Array.of_list (List.map register_tr tr)) (Array.of_list (List.map register_pr pr)) de in
	let retit = parse_lts has_header has_init do_add (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !lts 
		then raise (TransitionSysParserCustomException "Init node out of range.")
		else if not (let (_, _, _, v) = !lts.(!init_value) in v)
		then raise (TransitionSysParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (TransitionSysParserCustomException "Initial node statement missing.");
	(!init_value, retit)

	
let parse_explicit_lts in_channel =
	snd (parse_explicit_lts' in_channel false)
	
let parse_explicit_initlts in_channel =
	parse_explicit_lts' in_channel true


let parse_ts has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__ts_has_header := __def_ts_has_header;
		__ts_has_start := __def_ts_has_start;
		__ts_add_node := __def_ts_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (TransitionSysParserException (s, l, c))
	);
	__ts_has_header := has_header;
	__ts_has_start := has_start;
	__ts_add_node := add_func;
	(try
		Tcstsparser.ts Tcstslexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	TransitionSysParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	
let parse_explicit_ts' in_channel init_required =
	let init_value = ref (-1) in
	let ts = ref [||] in
	let props = ref TreeMap.empty_def in
	let props_count = ref 0 in
	let register_pr s =
		try
			TreeMap.find s !props
		with
			Not_found -> (
				props := TreeMap.add s !props_count !props;
				incr props_count;
				!props_count - 1
			)
	in
	let map_to_array map count = 
		let a = Array.make count "" in
		TreeMap.iter (fun s i -> a.(i) <- s) map;
		a
	in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i tr pr de =
		if (i < 0) || (i >= Array.length !ts)
		then raise (TransitionSysParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if let (_, _, _, v) = !ts.(i) in v
		then raise (TransitionSysParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!ts.(i) <- (pr, tr, de, true)
	in
	let add_func_old i tr pr de =
		node_list := (i, tr, pr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_ts _ = (map_to_array !props !props_count, !ts) in
	let ret_func_old _ = (
		ts := Array.make (!node_list_max + 1) ([||], [||], None, false);
		List.iter (fun (a,b,c,d) -> add_value a b c d) !node_list;
		ret_ts ()
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		ts := Array.make (n + 1) ([||], [||], None, false);
		add_func := add_value;
		ret_func := ret_ts
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (TransitionSysParserCustomException "Init node out of range.")
	)
	in
	let do_add i tr pr de = !add_func i (Array.of_list tr) (Array.of_list (List.map register_pr pr)) de in
	let retit = parse_ts has_header has_init do_add (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !ts 
		then raise (TransitionSysParserCustomException "Init node out of range.")
		else if not (let (_, _, _, v) = !ts.(!init_value) in v)
		then raise (TransitionSysParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (TransitionSysParserCustomException "Initial node statement missing.");
	(!init_value, retit)

	
let parse_explicit_ts in_channel =
	snd (parse_explicit_ts' in_channel false)
	
let parse_explicit_initts in_channel =
	parse_explicit_ts' in_channel true
