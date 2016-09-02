open Tcslist;;
open Tcsstrings;;

module SimpleArgs = struct
	type arg_spec = Arg.spec;;
	type arg_key = string list;;
	type arg_doc = Arg.doc;;

	let parseargv curr argv speclist anon_fun usage_msg print_help print_bad =
		let speclist' = List.fold_right (fun (keys, spec, doc) l ->
			List.fold_right (fun k l -> if ((k = "") || (k = "-") || (k = "--")) then l else (k, spec, doc)::l) keys l
		) speclist [] in
		try
			Arg.parse_argv ~current:curr argv speclist' anon_fun usage_msg
		with Arg.Help s -> (print_help usage_msg speclist; exit 1)
		   | Arg.Bad s -> (print_bad (String.sub s 0 (String.index s '\n')) usage_msg speclist; exit 1);;

	let parse speclist anon_fun usage_msg print_help print_bad =
		parseargv Arg.current Sys.argv speclist anon_fun usage_msg print_help print_bad

	let parsearr argv speclist anon_fun usage_msg print_help print_bad =
		parseargv (ref 0) (Array.of_list (""::(Array.to_list argv))) speclist anon_fun usage_msg print_help print_bad

	let parsestr str speclist anon_fun usage_msg print_help print_bad =
		parsearr (Array.of_list (StringUtils.explode str ' ')) speclist anon_fun usage_msg print_help print_bad

	let argprint_help usage_msg speclist =
		let keys_format = function
		|   (h::t) -> (List.fold_left (fun s i -> s ^ ", " ^ i) h t)
		|	_ -> "" in
		print_string (usage_msg ^ "\n");
		List.iter (fun (keys, spec, doc) ->
			print_string ("  " ^ keys_format keys ^ " " ^ doc ^ "\n")
		) speclist;
		print_string "\n";;

	let argprint_bad badstr usage_msg speclist =
		print_string (badstr ^ "\n\n");
		argprint_help usage_msg speclist;;

	let parsedef speclist anon_fun usage_msg = parse speclist anon_fun usage_msg argprint_help argprint_bad;;
end;;


module CustomArgs = struct

	type arg_type =
		Unit of (unit -> unit)
	|	Bool of (bool -> unit)
	|	String of (string -> unit)
	|	Int of (int -> unit)
	|	Float of (float -> unit)
	|	Option of (string * (unit -> unit)) list
	|	Tuple of arg_type list
	|	Multiple of (int -> (int -> string) -> int)

	type category' = {
		cat_name: string;
		cat_help_string: string;
	}
	
	type category = category' ref 

	type argument_item' = {
		main: arguments_object;
		optional: bool;
		multiple: bool;
		category: category;
		idents: string list;
		parent: argument_item;
		mutable persistent_children: argument_item list;
		mutable temporary_children: argument_item list;
		help_string: string;
		long_help: (unit -> string);
		arg_type: arg_type;
		mutable used: bool;
	}
	and argument_item = argument_item' ref
	and arguments_object' = {
		mutable running: bool;
		mutable help_mode: bool;
		title: string;
		main_help: string;
		ignore_case: bool;
		param_prefs: string list;
		root: argument_item;
		argument_hash: (string, argument_item) Hashtbl.t;
	}
	and arguments_object = arguments_object' ref
	
	type args_exception_type =
		ArgsUnknownArgument of string
	|	ArgsCustomFailure of string
	|	ArgsWrongParameter of argument_item * int * string * string (* argument, token index, given token, expected type *)
	|	ArgsRequiredParameter of argument_item * int * string (* parameter required but not given *)
	|	ArgsRequiredArgument of argument_item (* argument required but not given *)
	|	ArgsMultipleOccurrence of argument_item (* argument already occurred once *)

	exception ArgsException of args_exception_type

	let _no_cat = ref {
		cat_name = "";
		cat_help_string = "";
	}
	
	let new_object title main_help ignore_case param_prefs =
		let rec main = ref {
			running = false;
			help_mode = false;
			title = title;
			main_help = main_help;
			ignore_case = ignore_case;
			param_prefs = param_prefs;
			root = root_arg;
			argument_hash = Hashtbl.create 10;
		}
		and root_arg = ref {
			main = main;
			optional = true;
			multiple = false;
			category = _no_cat;
			idents = [];
			parent = root_arg;
			persistent_children = [];
			temporary_children = [];
			help_string = "";
			long_help = (fun _ -> "");
			arg_type = Unit (fun _ -> ());
			used = false;
		}
		in
			main
		
	let new_category name help_string = ref {
		cat_name = name;
		cat_help_string = help_string;
	}
	
	let _is_root_argument arg =
		!(!arg.main).root == !arg.parent
	
	let rec _argument_idents arg =
		if (_is_root_argument arg)
		then !arg.idents
		else List.fold_left (fun acc pident -> 
				List.fold_left (fun acc aident ->
					(pident ^ aident)::acc
				) acc !arg.idents
			 ) [] (_argument_idents !arg.parent)
			 
	let argument_ident arg =
		(List.hd !(!arg.main).param_prefs) ^ (List.hd (_argument_idents arg)) 

	let register_argument obj parent optional multiple category idents help_string long_help arg_type =
		let item = ref {
			main = obj;
			optional = optional;
			multiple = multiple;
			category = (match category with
							None -> _no_cat
						|	Some c -> c);
			idents = if !obj.ignore_case then List.map String.lowercase_ascii idents else idents;
			parent = (match parent with
							None -> !obj.root
						|	Some p -> p);
			persistent_children = [];
			temporary_children = [];
			help_string = help_string;
			long_help = long_help;
			arg_type = arg_type;
			used = false;
		}
		in
			if !obj.running
			then !(!item.parent).temporary_children <- item::!(!item.parent).temporary_children
			else !(!item.parent).persistent_children <- item::!(!item.parent).persistent_children;
			let ht = !obj.argument_hash in
			let argidents = _argument_idents item in
			List.iter (fun s ->
				if Hashtbl.mem ht s
				then failwith ("CustomArgs.register_argument: '" ^ s ^ "' already registered!");
				Hashtbl.add ht s item
			) argidents;
			item
			
	let format_arg_type = function
		Unit _ -> "unit"
	|	Bool _ -> "bool"
	|	String _ -> "string"
	|	Int _ -> "int"
	|	Float _ -> "float"
	|	Option l -> "one out of " ^ ListUtils.format (fun (s, _) -> s) l
	|	Tuple _ -> "tuple"
	|	Multiple _ -> "multiple"
			
	let format_exception = function
		ArgsUnknownArgument s -> "Unkown argument '" ^ s ^ "'."
	|	ArgsCustomFailure s -> s
	|	ArgsWrongParameter (arg, idx, token, exp_type) -> "Argument " ^ argument_ident arg ^ " requires parameter #" ^ string_of_int (idx + 1) ^ " to be " ^ exp_type ^ ", but read '" ^ token ^ "'."
	|	ArgsRequiredParameter (arg, idx, _) -> "Argument " ^ argument_ident arg ^ " requires parameter #" ^ string_of_int (idx + 1) ^ "."
	|	ArgsRequiredArgument arg -> "Argument " ^ argument_ident arg ^ " required but not given."
	|	ArgsMultipleOccurrence arg -> "Multiple occurrences of argument " ^ argument_ident arg ^ "."
			

			
	type toplevel_parse_type = TokenArg of argument_item | TokenStr of string

	let _parse_toplevel obj str = 
		let s = if !obj.ignore_case then String.lowercase_ascii str else str in
		let l = String.length s in
		let t = ref "" in
		let fnd = ref false in
		List.iter (fun p ->
			if not !fnd then (
				let pl = String.length p in
				if (l > pl) && (String.sub s 0 pl = p) then (
					fnd := true;
					t := String.sub s pl (l - pl)
				)
			)
		) !obj.param_prefs;
		if !fnd then (
			try
				TokenArg (Hashtbl.find !obj.argument_hash !t)
			with Not_found -> TokenStr str
		)
		else TokenStr str

	let _parse_parameters obj arg current args =
		let len = Array.length args in
		if !arg.used && (not !arg.multiple)
		then raise (ArgsException (ArgsMultipleOccurrence arg));
		!arg.used <- true;
		let cur_start = !current in
		let get_token arg_type =
			if !current < len then (
				incr current;
				args.(!current - 1)
			)
			else raise (ArgsException (ArgsRequiredParameter (arg, !current - cur_start, format_arg_type arg_type)))
		in
		
		let wrong_param_exception s a =
			raise (ArgsException (ArgsWrongParameter (arg, !current - cur_start - 1, s, format_arg_type a)))
		in
			
		let parse_bool s a =
			try
				bool_of_string s
			with Failure _ -> wrong_param_exception s a
		in

		let parse_string s a =
			s
		in
		
		let parse_int s a =
			try
				int_of_string s
			with Failure _ -> wrong_param_exception s a
		in
			
		let parse_float s a =
			try
				float_of_string s
			with Failure _ -> wrong_param_exception s a
		in
		
		let parse_multiple f =
			current := !current + f (Array.length args - !current) (fun i -> args.(!current + i))
		in
		
		let rec process_option_arg arg_type token l = match l with
			(s,f)::r -> if s = token then f () else process_option_arg arg_type token r
		|	[] -> wrong_param_exception token arg_type
		in
		
		let rec parse_arg_type arg_type = match arg_type with
			Unit f -> f ()
		|	Bool f -> f (parse_bool (get_token arg_type) arg_type)
		|	String f -> f (parse_string (get_token arg_type) arg_type)
		|	Int f -> f (parse_int (get_token arg_type) arg_type)
		|	Float f -> f (parse_float (get_token arg_type) arg_type)
		|	Option l -> process_option_arg arg_type (get_token arg_type) l
		|	Tuple l -> List.iter parse_arg_type l
		|	Multiple f -> parse_multiple f
		in
		
		parse_arg_type !arg.arg_type
		
		
	let parse_arguments obj args start =
		let ht = !obj.argument_hash in
		let current = ref start in
		let len = Array.length args in
	
		let init _ =
			!obj.running <- true;
		in
	
		let cleanup _ =
			let rec cleanup_arg arg =
				!arg.used <- false;
				List.iter cleanup_arg !arg.persistent_children;
				List.iter (fun a ->
					cleanup_arg a;
					List.iter (Hashtbl.remove ht) (_argument_idents a)
				) !arg.temporary_children;
				!arg.temporary_children <- []
			in
				!obj.running <- false;
				!obj.help_mode <- false;
				cleanup_arg !obj.root
		in
			
		let run _ =
			while !current < len do
				match _parse_toplevel obj args.(!current) with
					TokenArg arg -> (
						incr current;
						_parse_parameters obj arg current args
					)
				|	TokenStr str -> (
						try
							let arg = Hashtbl.find ht "" in
							_parse_parameters obj arg current args
						with Not_found ->
							raise (ArgsException (ArgsUnknownArgument str))
					)
			done;
		in
		
		let post_conditions () =
			let rec check_post_conditions arg =
				if (not !arg.optional) && (not !arg.used)
				then raise (ArgsException (ArgsRequiredArgument arg));
				List.iter check_post_conditions (!arg.persistent_children);
				List.iter check_post_conditions (!arg.temporary_children)
			in
				check_post_conditions !obj.root
		in
		
		try
			init ();
			run ();
			post_conditions ();
			cleanup ();
		with ArgsException e -> (
			cleanup ();
			raise (ArgsException e)
		)
		
	let parse_arguments_with_error obj args start error_func =
		try
			parse_arguments obj args start
		with ArgsException e -> error_func e

	let error_function obj printer except =
		printer (!obj.title ^ "\n\n");
		printer ("Command Line Error. " ^ (format_exception except) ^ "\n");
		exit 1
		
	let _get_long_help arg =
		let s = !arg.long_help () in
		if s = "" then !arg.help_string else s
		
	let help_function obj printer arglen args =
		let pref = List.hd !obj.param_prefs in
		if not !obj.help_mode then (
			!obj.help_mode <- true;
			let current = ref 0 in
			let arg = if arglen = 0 then !obj.root else (
				let arg = (match _parse_toplevel obj (args !current) with 
					TokenArg arg -> (
						incr current;
						arg
					)
				|	TokenStr str -> (
						try
							Hashtbl.find !obj.argument_hash ""
						with Not_found ->
							raise (ArgsException (ArgsUnknownArgument str))
					)
				) in
				if !current < arglen
				then _parse_parameters obj arg current (Array.init (arglen - !current) (fun i -> args (i + !current)));
				arg
			) in
			printer (!obj.title ^ "\n\n");
			if arg == !obj.root
			then printer (!obj.main_help ^ "\n\n")
			else (
				let l = ListUtils.format_plain (fun s -> s) (ListUtils.init !current args) in 
			    printer ("Help on " ^ l ^ "\n\n");
				printer (_get_long_help arg ^ "\n\n");
			);
			let children = !arg.persistent_children @ !arg.temporary_children in
			if children != [] then (
				printer "Options are\n";
				let compare_arg arg1 arg2 =
					let cat1 = !arg1.category in
					let cat2 = !arg2.category in
					if cat1 == cat2
					then compare (argument_ident arg1) (argument_ident arg2)
					else compare !cat1.cat_name !cat2.cat_name
				in
				let children = List.sort compare_arg children in
				let current_cat = ref _no_cat in
				let nice_print s indent =
					printer (StringUtils.break_and_indent s indent (79 - indent))
				in
				List.iter (fun arg' -> 
					if !current_cat != !arg'.category then (
						current_cat := !arg'.category;
						nice_print ("\n" ^ !(!current_cat).cat_help_string ^ "\n") 2;
					);
					let indent = if !current_cat == _no_cat then 2 else 4 in
					let idents = List.map (fun s -> pref ^ s) (_argument_idents arg') in
					let idents_str = ListUtils.custom_format (fun s -> s) "" "" ", " idents in
					let idents_str' = StringUtils.break idents_str 20 ' ' in
					let space = let len = StringUtils.last_line_length idents_str' in
					            if len < 20 then String.make (20 - len) ' ' else "" in
					let help_str = !arg'.help_string in
					let full_str = idents_str' ^ space ^ help_str ^ "\n" in
					nice_print full_str indent
				) children;
			)
		)

	let register_help_function obj idents help_string long_help printer =
		let f x y =
			help_function obj printer x y;
			exit 0
		in
			register_argument obj None true false None idents help_string long_help (Multiple f)
		
end;;
