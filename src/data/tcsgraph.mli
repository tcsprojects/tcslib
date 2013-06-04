open Tcslist;;
open Tcsset;;
open Tcsbasedata;;


module GraphUtils : sig
	
	type 'a undirected_root_graph = 'a * ('a -> 'a Iterators.iterator)
	
	type 'a directed_root_graph = 'a * ('a -> 'a Iterators.iterator) * ('a -> 'a Iterators.iterator)

	type 'a occurrence = 'a -> bool
	
	val is_reachable2: 'a occurrence -> 'a undirected_root_graph -> ('a -> bool) -> bool
	
	val is_reachable: 'a undirected_root_graph -> ('a -> bool) -> bool
	
	val iterate_with_minimal_distance2: 'a occurrence -> 'a undirected_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_minimal_distance: 'a undirected_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_maximal_distance_single_loop2: 'a occurrence -> 'a directed_root_graph -> ('a -> int -> unit) -> unit
	
	val iterate_with_maximal_distance_single_loop: 'a directed_root_graph -> ('a -> int -> unit) -> unit

	val build_reachability_set2: ('a -> 'a -> int) -> 'a undirected_root_graph -> 'a TreeSet.t

	val build_reachability_set: 'a undirected_root_graph -> 'a TreeSet.t

end



module DynamicGraph : sig

    type 'a dynamic_graph

    type comparison_func = int -> int -> int

    val make: unit -> 'a dynamic_graph

    val add_cmp: comparison_func -> 'a dynamic_graph -> int

    val copy_graph: 'a dynamic_graph -> 'a dynamic_graph

    val head_copy: 'a dynamic_graph -> 'a dynamic_graph

    val size: 'a dynamic_graph -> int

    val has_node: int -> 'a dynamic_graph -> bool

    val has_edge: int -> int -> 'a dynamic_graph -> bool

    val get_node_data: int -> 'a dynamic_graph -> 'a

    val set_node_data: int -> 'a -> 'a dynamic_graph -> unit

	val get_node_pred: int -> 'a dynamic_graph -> int TreeSet.t

	val get_node_succ: int -> 'a dynamic_graph -> int TreeSet.t

    val add_node: int -> 'a -> 'a dynamic_graph -> unit

    val del_node: int -> 'a dynamic_graph -> unit

    val add_edge: int -> int -> 'a dynamic_graph -> unit

    val del_edge: int -> int -> 'a dynamic_graph -> unit

    val iter: (int -> ('a * int TreeSet.t * int TreeSet.t) -> unit) -> 'a dynamic_graph -> unit

    val iter_by: (int -> unit) -> int -> 'a dynamic_graph -> unit
    
    val sub_graph_by_edge_pred: (int -> int -> bool) -> 'a dynamic_graph -> 'a dynamic_graph

	val sub_graph_by_node_closure: int -> (int -> (int -> unit) -> unit) -> 'a dynamic_graph -> 'a dynamic_graph

	val depth_find: int -> (int -> bool) -> (int -> bool)-> 'a dynamic_graph -> bool

end


module Digraph : sig

	type 'a node
	
	type 'a nodes = ('a node) TreeSet.t
	
	type 'a digraph
	
	val compare_node: 'a node -> 'a node -> int
	
	val equal_node: 'a node -> 'a node -> bool
	
	val empty_nodes: unit -> 'a nodes
	
	val create: unit -> 'a digraph

	val node_count: 'a digraph -> int
	val edge_count: 'a digraph -> int
	
	val add_node: 'a digraph -> 'a -> 'a node
	val add_empty_node: 'a digraph -> 'a node
	val del_node: 'a digraph -> 'a node -> unit
	
	val add_edge: 'a digraph -> 'a node -> 'a node -> unit
	val del_edge: 'a digraph -> 'a node -> 'a node -> unit
	
	val get_content: 'a node -> 'a
	val set_content: 'a node -> 'a -> unit
	
	val get_fwd_edges: 'a node -> 'a nodes
	val get_bwd_edges: 'a node -> 'a nodes
	
	val get_nodes: 'a digraph -> 'a nodes
	
end
