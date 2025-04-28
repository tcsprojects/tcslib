open Tcsgameparserinternal;;

exception GameParserException of string * int * int

exception GameParserCustomException of string

let parse_parity_game has_header has_start add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__pg_has_header := __def_pg_has_header;
		__pg_has_start := __def_pg_has_start;
		__pg_add_node := __def_pg_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (GameParserException (s, l, c))
	);
	__pg_has_header := has_header;
	__pg_has_start := has_start;
	__pg_add_node := add_func;
	(try
		Tcsparitygameparser.game Tcsparitygamelexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	GameParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	

let parse_explicit_pg' in_channel init_required =
	let init_value = ref (0) in
	let game = ref [||] in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i pr pl tr de =
		if (i < 0) || (i >= Array.length !game)
		then raise (GameParserCustomException ("Node out of range: " ^ (string_of_int i)));
		let (pr', _, _, _) = !game.(i) in
		if pr' >= 0
		then raise (GameParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!game.(i) <- (pr, pl, Array.of_list tr, de)
	in
	let add_func_old i pr pl tr de =
		node_list := (i, pr, pl, tr, de)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_func_old _ = (
		game := Array.make (!node_list_max + 1) (-1, -1, [||], "");
		List.iter (fun (a,b,c,d,e) -> add_value a b c d e) !node_list;
		!game
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		game := Array.make (n + 1) (-1, -1, [||], "");
		add_func := add_value;
		ret_func := (fun _ -> !game)
	)
	in
	let has_init i = (
		init_value := i;
		if (i < 0) then raise (GameParserCustomException "Init node out of range.")
	)
	in
	let retgame = parse_parity_game has_header has_init (fun a b c d e -> !add_func a b c d e) (fun a -> !ret_func a) in_channel in
	if !init_value >= 0 then (
		if !init_value >= Array.length !game 
		then raise (GameParserCustomException "Init node out of range.")
		else if (let (pr, _, _, _) = !game.(!init_value) in pr < 0)
		then raise (GameParserCustomException "Init node is not existing.")
	)
	else if init_required then raise (GameParserCustomException "Initial node statement missing.");
	(!init_value, retgame)

	
let parse_explicit_pg in_channel =
	snd (parse_explicit_pg' in_channel false)
	
let parse_explicit_initpg in_channel =
	parse_explicit_pg' in_channel false

let parse_parity_solution has_header add_func ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__pgsol_has_header := __def_pgsol_has_header;
		__pgsol_add_node := __def_pgsol_add_node;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (GameParserException (s, l, c))
	);
	__pgsol_has_header := has_header;
	__pgsol_add_node := add_func;
	(try
		Tcsparitysolutionparser.sol Tcsparitysolutionlexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	GameParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()


let parse_explicit_parity_solution in_channel =
	let (sol, strat) = (ref [||], ref [||]) in
	let node_list = ref [] in
	let node_list_max = ref (-1) in
	let add_value i pl str =
		if (i < 0) || (i >= Array.length !sol)
		then raise (GameParserCustomException ("Node out of range: " ^ (string_of_int i)));
		if !sol.(i) >= 0
		then raise (GameParserCustomException ("Node already defined: " ^ (string_of_int i)));
		!sol.(i) <- pl;
		(match str with
			None -> ()
		|	Some j -> !strat.(i) <- j)
	in
	let add_func_old i pl str =
		node_list := (i, pl, str)::!node_list;
		node_list_max := max !node_list_max i
	in
	let add_func = ref add_func_old in
	let ret_func_old _ = (
		let n = !node_list_max + 1 in
		sol := Array.make n (-1);
		strat := Array.make n (-1);
		List.iter (fun (a,b,c) -> add_value a b c) !node_list;
		(!sol, !strat)
	) in
	let ret_func = ref ret_func_old in
	let has_header n = (
		sol := Array.make (n+1) (-1);
		strat := Array.make (n+1) (-1);
		add_func := add_value;
		ret_func := (fun _ -> (!sol, !strat))
	)
	in
	parse_parity_solution has_header (fun a b c -> !add_func a b c) (fun a -> !ret_func a) in_channel
