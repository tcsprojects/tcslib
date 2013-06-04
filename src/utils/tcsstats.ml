open Tcsstrings;;
open Tcstiming;;

exception StatsException of string

module CustomStats = struct

	type stats_data =
		StatsInt of int ref
	|	StatsBool of bool ref
	|	StatsString of string ref
	|	StatsFloat of float ref * (float -> string)
	|	StatsRational of int ref * (int ref) * (int -> int -> string)
	|	StatsTiming of SimpleTiming.timing_object
	|	StatsCustom of (unit -> string) ref

	type stats_item' = {
		ident: string;
		parent: stats_category;
		data: stats_data;
	}
	and stats_item = stats_item' ref
	and stats_category' = {
		cat_ident: string;
		information: string;
		parent_cat: stats_category;
		sub_stats: (string, stats_item) Hashtbl.t;
		sub_cats: (string, stats_category) Hashtbl.t;
	}
	and stats_category = stats_category' ref

	let rec _no_cat = ref {
		cat_ident = "";
		information = "";
		parent_cat = _no_cat;
		sub_stats = Hashtbl.create 1;
		sub_cats = Hashtbl.create 1;
	}

	let get_category_ident cat = !cat.cat_ident

	let rec get_category_full_ident cat =
		if !cat.parent_cat == _no_cat
		then get_category_ident  cat
		else get_category_full_ident !cat.parent_cat ^ "." ^ get_category_ident cat

	let get_category_information cat = !cat.information

	let get_sub_stats cat = Hashtbl.fold (fun _ el acc -> el::acc) !cat.sub_stats []

	let get_sub_cats cat = Hashtbl.fold (fun _ el acc -> el::acc) !cat.sub_cats []

	let get_ident item = !item.ident

	let get_full_ident item = get_category_full_ident !item.parent ^ "." ^ get_ident item

	let get_data item = !item.data

	let format_data = function
		StatsInt i -> string_of_int !i
	|	StatsBool b -> string_of_bool !b
	|	StatsString s -> !s
	|	StatsFloat (f, fmt) -> fmt !f
	|	StatsRational (a, b, fmt) -> fmt !a !b
	|	StatsTiming t -> SimpleTiming.format t
	|	StatsCustom f -> !f ()

	let new_int init = StatsInt (ref init)

	let read_int item = match !item.data with StatsInt i -> !i
	| _ -> raise (StatsException "CustomStats.read_int: Trying to read other type.")

	let write_int item j = match !item.data with StatsInt i -> i := j
	| _ -> raise (StatsException "CustomStats.write_int: Trying to write other type.")

	let get_data_int = function StatsInt i -> i
	| _ -> raise (StatsException "CustomStats.get_data_int: Trying to read other type.")

	let new_bool init = StatsBool (ref init)

	let read_bool item = match !item.data with StatsBool b -> !b
	| _ -> raise (StatsException "CustomStats.read_bool: Trying to read other type.")

	let write_bool item j = match !item.data with StatsBool b -> b := j
	| _ -> raise (StatsException "CustomStats.write_bool: Trying to write other type.")

	let get_data_bool = function StatsBool b -> b
	| _ -> raise (StatsException "CustomStats.get_data_bool: Trying to read other type.")

	let new_string init = StatsString (ref init)

	let read_string item = match !item.data with StatsString s -> !s
	| _ -> raise (StatsException "CustomStats.read_string: Trying to read other type.")

	let write_string item j = match !item.data with StatsString s -> s := j
	| _ -> raise (StatsException "CustomStats.write_string: Trying to write other type.")

	let get_data_string = function StatsString s -> s
	| _ -> raise (StatsException "CustomStats.get_data_string: Trying to read other type.")

	let new_float init = StatsFloat (ref init, Printf.sprintf "%2.f")

	let new_float2 init f = StatsFloat (ref init, f)

	let read_float item = match !item.data with StatsFloat (f, _) -> !f
	| _ -> raise (StatsException "CustomStats.read_float: Trying to read other type.")

	let write_float item j = match !item.data with StatsFloat (f, _) -> f := j
	| _ -> raise (StatsException "CustomStats.write_float: Trying to write other type.")

	let get_data_float = function StatsFloat (f, _) -> f
	| _ -> raise (StatsException "CustomStats.get_data_float: Trying to read other type.")

	let new_rational init1 init2 =
		StatsRational (ref init1, ref init2,
		               fun a b -> Printf.sprintf "%2.f" ((float a) /. (float b)))

	let new_rational2 init1 init2 f = StatsRational (ref init1, ref init2, f)

	let read_rational_nom item = match !item.data with StatsRational (n, _, _) -> !n
	| _ -> raise (StatsException "CustomStats.read_rational_nom: Trying to read other type.")

	let write_rational_nom item j = match !item.data with StatsRational (n, _, _) -> n := j
	| _ -> raise (StatsException "CustomStats.write_rational_nom: Trying to write other type.")

	let read_rational_denom item = match !item.data with StatsRational (_, d, _) -> !d
	| _ -> raise (StatsException "CustomStats.read_rational_denom: Trying to read other type.")

	let write_rational_denom item j = match !item.data with StatsRational (_, d, _) -> d := j
	| _ -> raise (StatsException "CustomStats.write_rational_denom: Trying to write other type.")

	let new_timing init = StatsTiming init

	let read_timing item = match !item.data with StatsTiming t -> t
	| _ -> raise (StatsException "CustomStats.read_timing: Trying to read other type.")

	let new_custom init = StatsCustom (ref init)

	let read_custom item = match !item.data with StatsCustom b -> !b
	| _ -> raise (StatsException "CustomStats.read_custom: Trying to read other type.")

	let write_custom item j = match !item.data with StatsCustom b -> b := j
	| _ -> raise (StatsException "CustomStats.write_custom: Trying to write other type.")

	let new_root_category ident information = ref {
		cat_ident = ident;
		information = information;
		parent_cat = _no_cat;
		sub_stats = Hashtbl.create 10;
		sub_cats = Hashtbl.create 10;
	}

	let new_sub_category cat ident information =
		if (Hashtbl.mem !cat.sub_stats ident) || (Hashtbl.mem !cat.sub_cats ident)
		then raise (StatsException ("CustomStats.new_sub_category: Sub item '" ^ ident ^ "' already registered."));
		let subcat = ref {
			cat_ident = ident;
			information = information;
			parent_cat = cat;
			sub_stats = Hashtbl.create 10;
			sub_cats = Hashtbl.create 10;
		} in
		Hashtbl.add !cat.sub_cats ident subcat;
		subcat

	let new_sub_stat cat ident data =
		if (Hashtbl.mem !cat.sub_stats ident) || (Hashtbl.mem !cat.sub_cats ident)
		then raise (StatsException ("CustomStats.new_sub_stat: Sub item '" ^ ident ^ "' already registered."));
		let substat = ref {
			ident = ident;
			parent = cat;
			data = data;
		} in
		Hashtbl.add !cat.sub_stats ident substat;
		substat

	let get_sub_cat cat ident =
		Hashtbl.find !cat.sub_cats ident

	let get_sub_item cat ident =
		Hashtbl.find !cat.sub_stats ident

	let print_stat printer item =
		printer (get_full_ident item ^ ": " ^ format_data !item.data)

	let rec print_cat printer item =
		if Hashtbl.length !item.sub_stats > 0 then (
			let full_ident = get_category_full_ident item in
			let c = 79 in
			printer (full_ident ^ "\n");
			printer (get_category_information item ^ "\n");
			printer (String.make c '=' ^ "\n");
			let m = Hashtbl.fold (fun ident _ m -> max m (String.length ident)) !item.sub_stats 0 in
			let len = String.length full_ident + m + 3 in
			Hashtbl.iter (fun ident stat ->
				printer (StringUtils.fillup (full_ident ^ "." ^ ident ^ ": ") len ' ' ^ format_data !stat.data)
			) !item.sub_stats;
			printer "\n";
		);
		Hashtbl.iter (fun _ -> print_cat printer) !item.sub_cats

end;;
