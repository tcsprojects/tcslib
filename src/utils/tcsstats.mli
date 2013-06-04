open Tcstiming

exception StatsException of string

module CustomStats :
  sig
  
    type stats_data =
        StatsInt of int ref
      | StatsBool of bool ref
      | StatsString of string ref
      | StatsFloat of float ref * (float -> string)
      | StatsRational of int ref * int ref * (int -> int -> string)
	  | StatsTiming of SimpleTiming.timing_object
      | StatsCustom of (unit -> string) ref
	  
    type stats_item
	
    type stats_category
	
    val get_category_ident : stats_category -> string
	
    val get_category_full_ident : stats_category -> string
	
    val get_category_information : stats_category -> string
	
    val get_sub_stats : stats_category -> stats_item list
	
    val get_sub_cats : stats_category -> stats_category list
	
    val get_ident : stats_item -> string
	
    val get_full_ident : stats_item -> string
    
	val get_data : stats_item -> stats_data
    
	val format_data : stats_data -> string
    
	val new_int : int -> stats_data
    val read_int : stats_item -> int
    val write_int : stats_item -> int -> unit
	val get_data_int : stats_data -> int ref
    
	val new_bool : bool -> stats_data
    val read_bool : stats_item -> bool
    val write_bool : stats_item -> bool -> unit
	val get_data_bool : stats_data -> bool ref
    
	val new_string : string -> stats_data
    val read_string : stats_item -> string
    val write_string : stats_item -> string -> unit
	val get_data_string: stats_data -> string ref
    
	val new_float : float -> stats_data
    val new_float2 : float -> (float -> string) -> stats_data
    val read_float : stats_item -> float
    val write_float : stats_item -> float -> unit
	val get_data_float : stats_data -> float ref
    
	val new_rational : int -> int -> stats_data
    val new_rational2 : int -> int -> (int -> int -> string) -> stats_data
    val read_rational_nom : stats_item -> int
    val write_rational_nom : stats_item -> int -> unit
    val read_rational_denom : stats_item -> int
    val write_rational_denom : stats_item -> int -> unit
    
	val new_timing : SimpleTiming.timing_object -> stats_data
    val read_timing : stats_item -> SimpleTiming.timing_object

	val new_custom : (unit -> string) -> stats_data
    val read_custom : stats_item -> unit -> string
    val write_custom : stats_item -> (unit -> string) -> unit
    
	val new_root_category : string -> string -> stats_category
    
	val new_sub_category :
      stats_category -> string -> string -> stats_category
    
	val new_sub_stat :
      stats_category -> string -> stats_data -> stats_item
    
	val get_sub_cat : stats_category -> string -> stats_category
    
	val get_sub_item : stats_category -> string -> stats_item
    
	val print_stat : (string -> unit) -> stats_item -> unit
    
	val print_cat : (string -> unit) -> stats_category -> unit
  end
