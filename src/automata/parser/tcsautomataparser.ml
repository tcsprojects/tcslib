open Tcsautomataparserinternal;;
open Tcsautomata;;
open Tcsbasedata;;
open Tcsset;;

exception AutomataParserException of string * int * int

exception AutomataParserCustomException of string


let parse_automaton automaton_type automaton_int_alphabet automaton_alphabet_add
                    automaton_int_stack automaton_stack_add
                    automaton_state_add automaton_initial_state
					automaton_transition_add automaton_transition_call_add automaton_transition_ret_add
					automaton_transition_epsilon_add
					ret_func in_channel =
	let cleanup () = (
		__parse_exception := __def_parse_exception;
		__lexer_line := 0;
		__lexer_character := 0;
		__automaton_type := __def_automaton_type;
		__automaton_int_alphabet := __def_automaton_int_alphabet;
		__automaton_alphabet_add := __def_automaton_alphabet_add;
		__automaton_int_stack := __def_automaton_int_stack;
		__automaton_stack_add := __def_automaton_stack_add;
		__automaton_state_add := __def_automaton_state_add;
		__automaton_initial_state := __def_automaton_initial_state;
		__automaton_transition_add := __def_automaton_transition_add;
		__automaton_transition_call_add := __def_automaton_transition_call_add;
		__automaton_transition_ret_add := __def_automaton_transition_ret_add;
	)
	in
	__parse_exception := (fun s ->
		let (l, c) = (!__lexer_line, !__lexer_character) in
		cleanup ();
		raise (AutomataParserException (s, l, c))
	);
	__automaton_type := automaton_type;
	__automaton_int_alphabet := automaton_int_alphabet;
	__automaton_alphabet_add := automaton_alphabet_add;
	__automaton_int_stack := automaton_int_stack;
	__automaton_stack_add := automaton_stack_add;
	__automaton_state_add := automaton_state_add;
	__automaton_initial_state := automaton_initial_state;
	__automaton_transition_add := automaton_transition_add;
	__automaton_transition_call_add := automaton_transition_call_add;
	__automaton_transition_ret_add := automaton_transition_ret_add;
	__automaton_transition_epsilon_add := automaton_transition_epsilon_add;
	(try
		Tcsautoparser.auto Tcsautomatalexer.token (Lexing.from_channel in_channel)
	with 
		Invalid_argument s -> !__parse_exception ("Invalid Argument: " ^ s)
	|	AutomataParserCustomException s -> !__parse_exception s
	|	Parsing.Parse_error -> !__parse_exception ("Parse error.")
	);
	cleanup ();
	ret_func ()
	

type automata_type =
	NbaType of (int, int) NBA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	DbaType of (int, int) DBA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	NpaType of (int, int) NPA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	DpaType of (int, int) DPA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	NbvpaType of (int, int, int) NBVPA.t * int NMAFunctions.state_iterator * (int NMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	DbvpaType of (int, int, int) DBVPA.t * int NMAFunctions.state_iterator * (int DMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	NpvpaType of (int, int, int) NPVPA.t * int NMAFunctions.state_iterator * (int NMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	DpvpaType of (int, int, int) DPVPA.t * int NMAFunctions.state_iterator * (int DMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator


let parse_automaton2 in_channel =

	let automaton_type_data = ref "" in
	let automaton_type str = automaton_type_data := String.lowercase str in

	let alphabet_int = ref None in
	let automaton_int_alphabet size = alphabet_int := Some size in
	
	let alphabet_list = ref [] in
	let automaton_alphabet_add idx ann kind = alphabet_list := (idx, ann, kind)::!alphabet_list in
	
	let stack_int = ref None in
	let automaton_int_stack size = stack_int := Some size in
	
	let stack_list = ref [] in
	let automaton_stack_add idx ann = stack_list := (idx, ann)::!stack_list in

	let state_list = ref [] in
	let first_state = ref (-1) in
	let automaton_state_add idx prio ann =
		state_list := (idx, prio, ann)::!state_list;
		if !first_state = -1
		then first_state := idx
	in
	
	let initial = ref None in
	let automaton_initial_state idx = initial := Some idx in
	
	let transition_list = ref [] in
	let automaton_transition_add idx al tar = transition_list := (idx,al,TreeSet.of_list_def tar)::!transition_list in
	
	let transition_call_list = ref [] in
	let automaton_transition_call_add idx al tar = transition_call_list := (idx,al,TreeSet.of_list_def tar)::!transition_call_list in

	let transition_ret_list = ref [] in
	let automaton_transition_ret_add idx al st tar = transition_ret_list := (idx,al,st,TreeSet.of_list_def tar)::!transition_ret_list in
	
	let transition_epsilon_list = ref [] in
	let automaton_transition_epsilon_add idx tar = transition_epsilon_list := (idx,TreeSet.of_list_def tar)::!transition_epsilon_list in
	
	let tree_map_list_merge_add key value tree_map =
		try
			let old_value = TreeMap.find key tree_map in
			TreeMap.add key (TreeSet.union value old_value) tree_map
		with
			Not_found -> TreeMap.add key value tree_map
	in

	let ret_func _ =
		while !transition_epsilon_list != [] do
			let (s,r) = List.hd !transition_epsilon_list in
			transition_epsilon_list := List.tl !transition_epsilon_list;
			transition_epsilon_list := List.map (fun (q,t) ->
				(q, TreeSet.fold (fun u acc ->
					if u = s then TreeSet.union r (TreeSet.add u acc) else TreeSet.add u acc
				) t TreeSet.empty_def)
			) !transition_epsilon_list;
			transition_list := List.map (fun (q,a,t) ->
				(q, a,TreeSet.fold (fun u acc ->
					if u = s then TreeSet.union r (TreeSet.add u acc) else TreeSet.add u acc
				) t TreeSet.empty_def)
			) !transition_list;
			transition_call_list := List.map (fun (q,a,t) ->
				(q, a, TreeSet.fold (fun (u,b) acc ->
					if u = s then TreeSet.union (TreeSet.map2_def (fun x -> (x,b)) r) (TreeSet.add (u,b) acc) else TreeSet.add (u,b) acc
				) t TreeSet.empty_def)
			) !transition_call_list;
			transition_ret_list := List.map (fun (q,a,b,t) ->
				(q, a, b, TreeSet.fold (fun u acc ->
					if u = s then TreeSet.union r (TreeSet.add u acc) else TreeSet.add u acc
				) t TreeSet.empty_def)
			) !transition_ret_list;
		done;
		let has_stack = (!automaton_type_data = "nbvpa") || (!automaton_type_data = "dbvpa") || (!automaton_type_data = "npvpa") || (!automaton_type_data = "dpvpa") in
		if has_stack then (
			let (alphabet_compare, alphabet_iterator, alphabet_format) =
				match !alphabet_int with
					None -> (
						let f i = function AlphabetInternal -> NMVPA.Internal i | AlphabetCall -> NMVPA.Push i | AlphabetRet -> NMVPA.Pop i in
						let g s = function AlphabetInternal -> s | AlphabetCall -> "<" ^ s | AlphabetRet -> s ^ ">" in
						let m = ref TreeMap.empty_def in
						List.iter (fun (i,s,k) -> m := TreeMap.add (f i k) (g s k) !m) !alphabet_list;
						(compare,
						 TreeMap.to_key_iterator !m,
						 (fun i -> TreeMap.find i !m))
					)
				|	Some size -> (
						(compare,
						 Iterators.of_array (Array.init size (fun i -> NMVPA.Internal i)),
						 (function (NMVPA.Internal i) -> string_of_int i | _ -> failwith "impossible"))
					)
			in
			let (stack_compare, stack_iterator, stack_format) =
				match !stack_int with
					None -> (
						let m = ref TreeMap.empty_def in
						List.iter (fun (i,s) -> m := TreeMap.add i s !m) !stack_list;
						(compare,
						 TreeMap.to_key_iterator !m,
						 (fun i -> TreeMap.find i !m))
					)
				|	Some size -> (
						(compare,
						 Iterators.of_array (Array.init size (fun i -> i)),
						 string_of_int)
					)
			in
			let (states_compare, states_iterator, states_format, states_omega) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,p,s) -> m := TreeMap.add i (p,s) !m) !state_list;
				(compare,
				 TreeMap.to_key_iterator !m,
				 (fun i -> OptionUtils.resolve (snd (TreeMap.find i !m)) (fun s -> s) (string_of_int i)),
				 (fun i -> fst (TreeMap.find i !m)))
			in
			let initial_state =
				match !initial with
					Some i -> i
				|	None -> !first_state
			in
			let (delta, delta_singleton) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,a,l) -> m := tree_map_list_merge_add (i,a) l !m) !transition_list;
				let delta i = function
					NMVPA.Internal a -> (
						try
							TreeSet.to_iterator (TreeMap.find (i,a) !m)
						with
							Not_found -> Iterators.of_list []
					) |	_ -> failwith "wrong alphabet symbol"
				in
				let delta_singleton i = function
					NMVPA.Internal a -> (
						try
							match TreeSet.elements (TreeMap.find (i,a) !m) with
								[x] -> x
							|	_ -> raise (AutomataParserCustomException ("Automaton is not deterministic"))
						with
							Not_found -> raise (AutomataParserCustomException ("Automaton is not total"))
					) |	_ -> failwith "wrong alphabet symbol"
				in
				(delta, delta_singleton)
			in
			let (delta_call, delta_call_singleton) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,a,l) -> m := tree_map_list_merge_add (i,a) l !m) !transition_call_list;
				let delta_call i = function
					NMVPA.Push a -> (
						try
							TreeSet.to_iterator (TreeMap.find (i,a) !m)
						with
							Not_found -> Iterators.of_list []
					) |	_ -> failwith "wrong alphabet symbol"
				in
				let delta_call_singleton i = function
					NMVPA.Push a -> (
						try
							match TreeSet.elements (TreeMap.find (i,a) !m) with
								[x] -> x
							|	_ -> raise (AutomataParserCustomException ("Automaton is not deterministic"))
						with
							Not_found -> raise (AutomataParserCustomException ("Automaton is not total"))
					) |	_ -> failwith "wrong alphabet symbol"
				in
				(delta_call, delta_call_singleton)
			in
			let (delta_ret, delta_ret_singleton) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,a,s,l) -> m := tree_map_list_merge_add (i,a,s) l !m) !transition_ret_list;
				let delta_ret i a s = match a with
					NMVPA.Pop a -> (
						try
							TreeSet.to_iterator (TreeMap.find (i,a,s) !m)
						with
							Not_found -> Iterators.of_list []
					) |	_ -> failwith "wrong alphabet symbol"
				in
				let delta_ret_singleton i a s = match a with
					NMVPA.Pop a -> (
						try
							match TreeSet.elements (TreeMap.find (i,a,s) !m) with
								[x] -> x
							|	_ -> raise (AutomataParserCustomException ("Automaton is not deterministic"))
						with
							Not_found -> raise (AutomataParserCustomException ("Automaton is not total"))
					) |	_ -> failwith "wrong alphabet symbol"
				in
				(delta_ret, delta_ret_singleton)
			in
			
			let alphabet_domain = Domain.make alphabet_compare alphabet_format in
			let stack_domain = Domain.make stack_compare stack_format in
			let states_domain = Domain.make states_compare states_format in
			
			if !automaton_type_data = "nbvpa" then (
				let nbvpa = NBVPA.build states_domain alphabet_domain stack_domain initial_state
										delta delta_call delta_ret (fun s -> (states_omega s) mod 2 = 0)
				in
				NbvpaType (nbvpa, states_iterator, alphabet_iterator, stack_iterator)
			)
			else if !automaton_type_data = "dbvpa" then (
				let dbvpa = DBVPA.build states_domain alphabet_domain stack_domain initial_state
										delta_singleton delta_call_singleton delta_ret_singleton (fun s -> (states_omega s) mod 2 = 0)
				in
				DbvpaType (dbvpa, states_iterator, alphabet_iterator, stack_iterator)
			)
			else if !automaton_type_data = "npvpa" then (
				let npvpa = NPVPA.build states_domain alphabet_domain stack_domain initial_state
										delta delta_call delta_ret states_omega
				in
				NpvpaType (npvpa, states_iterator, alphabet_iterator, stack_iterator)
			)
			else if !automaton_type_data = "dpvpa" then (
				let dpvpa = DPVPA.build states_domain alphabet_domain stack_domain initial_state
										delta_singleton delta_call_singleton delta_ret_singleton states_omega
				in
				DpvpaType (dpvpa, states_iterator, alphabet_iterator, stack_iterator)
			)
			else raise (AutomataParserCustomException ("Unknown type: " ^ !automaton_type_data))
		)
		else (
			let (alphabet_compare, alphabet_iterator, alphabet_format) =
				match !alphabet_int with
					None -> (
						let m = ref TreeMap.empty_def in
						List.iter (fun (i,s,_) -> m := TreeMap.add i s !m) !alphabet_list;
						(compare,
						 TreeMap.to_key_iterator !m,
						 (fun i -> TreeMap.find i !m))
					)
				|	Some size -> (
						(compare,
						 Iterators.of_array (Array.init size (fun i -> i)),
						 string_of_int)
					)
			in
			let (states_compare, states_iterator, states_format, states_omega) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,p,s) -> m := TreeMap.add i (p,s) !m) !state_list;
				(compare,
				 TreeMap.to_key_iterator !m,
				 (fun i -> OptionUtils.resolve (snd (TreeMap.find i !m)) (fun s -> s) (string_of_int i)),
				 (fun i -> fst (TreeMap.find i !m)))
			in
			let initial_state =
				match !initial with
					Some i -> i
				|	None -> !first_state
			in
			let (delta, delta_singleton) =
				let m = ref TreeMap.empty_def in
				List.iter (fun (i,a,l) -> m := tree_map_list_merge_add (i,a) l !m) !transition_list;
				let delta i a =
					try
						TreeSet.to_iterator (TreeMap.find (i,a) !m)
					with
						Not_found -> Iterators.of_list []
				in
				let delta_singleton i a =
					try
						match TreeSet.elements (TreeMap.find (i,a) !m) with
							[x] -> x
						|	_ -> raise (AutomataParserCustomException ("Automaton is not deterministic"))
					with
						Not_found -> raise (AutomataParserCustomException ("Automaton is not total"))
				in
				(delta, delta_singleton)
			in

			let alphabet_domain = Domain.make alphabet_compare alphabet_format in
			let states_domain = Domain.make states_compare states_format in
			
			if !automaton_type_data = "nba" then (
				if (!stack_list != []) || (!transition_call_list != []) || (!transition_ret_list != [])
				then raise (AutomataParserCustomException ("NBAs do not have stacks"));
				let nba = NBA.build states_domain alphabet_domain initial_state
									delta (fun s -> (states_omega s) mod 2 = 0)
				in
				NbaType (nba, states_iterator, alphabet_iterator)
			)
			else if !automaton_type_data = "dba" then (
				if (!stack_list != []) || (!transition_call_list != []) || (!transition_ret_list != [])
				then raise (AutomataParserCustomException ("DBAs do not have stacks"));
				let dba = DBA.build states_domain alphabet_domain initial_state
									delta_singleton (fun s -> (states_omega s) mod 2 = 0)
				in
				DbaType (dba, states_iterator, alphabet_iterator)
			)
			else if !automaton_type_data = "npa" then (
				if (!stack_list != []) || (!transition_call_list != []) || (!transition_ret_list != [])
				then raise (AutomataParserCustomException ("NPAs do not have stacks"));
				let npa = NPA.build states_domain alphabet_domain initial_state
									delta states_omega
				in
				NpaType (npa, states_iterator, alphabet_iterator)
			)
			else if !automaton_type_data = "dpa" then (
				if (!stack_list != []) || (!transition_call_list != []) || (!transition_ret_list != [])
				then raise (AutomataParserCustomException ("DPAs do not have stacks"));
				let dpa = DPA.build states_domain alphabet_domain initial_state
									delta_singleton states_omega
				in
				DpaType (dpa, states_iterator, alphabet_iterator)
			)
			else raise (AutomataParserCustomException ("Unknown type: " ^ !automaton_type_data))
		)
	in

	parse_automaton automaton_type automaton_int_alphabet automaton_alphabet_add
	                automaton_int_stack automaton_stack_add
                    automaton_state_add automaton_initial_state
					automaton_transition_add automaton_transition_call_add automaton_transition_ret_add
					automaton_transition_epsilon_add
					ret_func in_channel 	
