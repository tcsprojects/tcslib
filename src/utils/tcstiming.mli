module SimpleTiming :
  sig
    type timing_state = InactiveTimingObject | ActiveTimingObject
    type timing_object

    val init : bool -> timing_object
    val start : timing_object -> unit
    val stop : timing_object -> unit
    val state : timing_object -> timing_state
    val read : timing_object -> float
    val run_count : timing_object -> int
    val format : timing_object -> string
    val read_avg : timing_object -> float
    val format_avg : timing_object -> string
	val increment_by : timing_object -> timing_object -> unit
	val timed_function : timing_object -> ('a -> 'b) -> ('a -> 'b)
  end
  
module HierarchicalTiming :
  sig
    type hierarchical_timing_object

    val new_object : hierarchical_timing_object -> hierarchical_timing_object
    val new_root_object : unit -> hierarchical_timing_object
    val is_root_object : hierarchical_timing_object -> bool
    val dispose_object : hierarchical_timing_object -> unit
    val start : hierarchical_timing_object -> unit
    val stop : hierarchical_timing_object -> unit
    val state : hierarchical_timing_object -> SimpleTiming.timing_state
    val read : hierarchical_timing_object -> float
    val run_count : hierarchical_timing_object -> int
    val format : hierarchical_timing_object -> string
	val as_timing_object : hierarchical_timing_object -> SimpleTiming.timing_object
  end
	
	
module TimingProfiler :

  sig
		type t
		
		val init: t
		
		val enter: t -> string -> unit
		
		val leave: t -> string -> unit
		
		val format: t -> string
		
		val profile1: t -> string -> ('a -> 'b) -> 'a -> 'b
	
		val profile2: t -> string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

		val profile3: t -> string -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

		val profile4: t -> string -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e

		val profile5: t -> string -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f

end;;