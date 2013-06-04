open Tcslist;;
open Tcsarray;;
open Tcsset;;
open Tcsbasedata;;
open Tcscache;;



module GraphUtils = struct

	type 'a undirected_root_graph = 'a * ('a -> 'a Iterators.iterator)
	
	type 'a directed_root_graph = 'a * ('a -> 'a Iterators.iterator) * ('a -> 'a Iterators.iterator)
	
	type 'a occurrence = 'a -> bool
	
	let default_occurrence_func cmp =
		let base = ref (TreeSet.empty cmp) in
		fun x -> if TreeSet.mem x !base then true
		         else (base := TreeSet.add x !base; false)

	let rec is_reachable2 occ (init, iter) pred =
		if occ init then false else if pred init then true
		else Iterators.exists (iter init) (fun n -> is_reachable2 occ (n, iter) pred)
	
	let is_reachable (gr: 'a undirected_root_graph) =
		is_reachable2 (default_occurrence_func compare) gr
	
	let iterate_with_minimal_distance2 occ (init, iter) cb =
		let rec iterate i stack newstack =
			match stack with
				l::ls -> (
					match l with
						n::ns -> 
							if occ n then iterate i (ns::ls) newstack
							else (
								cb n i;
								iterate i (ns::ls) ((Iterators.to_list (iter n))::newstack)
							)
					|	[] -> iterate i ls newstack
				)
			|	[] -> (
				match newstack with
					[] -> ()
				|	_ -> iterate (i + 1) newstack []
			)
		in
			iterate 0 [[init]] []
			
	let iterate_with_minimal_distance (gr: 'a undirected_root_graph) =
		iterate_with_minimal_distance2 (default_occurrence_func compare) gr
		
	let iterate_with_maximal_distance_single_loop2 occ (init, prev, next) cb =
		let get_dist' u n =
			if n = init then 0
			             else 1 + ListUtils.max_elt compare (List.map u (Iterators.to_list (next n)))
		in
		let get_dist = RecursiveFunctionCache.cache_function get_dist' compare in
		let rec reach_next i n =
			if not (occ n) then (
				cb n (get_dist n);
				Iterators.iter (prev n) (reach_next (i + 1))
			)
		in
			reach_next 0 init
	
	let iterate_with_maximal_distance_single_loop (gr: 'a directed_root_graph) =
		iterate_with_maximal_distance_single_loop2 (default_occurrence_func compare) gr
			
	let build_reachability_set2 cmp (init, iter) =
		let s = ref (TreeSet.empty cmp) in
		let rec iterate n =
			if not (TreeSet.mem n !s) then (
				s := TreeSet.add n !s;
				Iterators.iter (iter n) iterate
			)
		in
			iterate init;
			!s
		
	let build_reachability_set (gr: 'a undirected_root_graph) =
		build_reachability_set2 compare gr

end




module DynamicGraph = struct

    type 'a dynamic_graph = ((int TreeSet.t) array) ref *
                             ((int, ('a * (int TreeSet.t) * (int TreeSet.t))) TreeMap.t) ref *
                             int ref

    type comparison_func = int -> int -> int

    let make _ =
        (ref [||], ref TreeMap.empty_def, ref 0)

    let add_cmp cmp (cmps, data, _) =
    	let s = TreeMap.fold (fun v _ -> TreeSet.add v) !data (TreeSet.empty cmp) in
        cmps := Array.append !cmps [|s|];
        Array.length !cmps - 1

    let copy_graph (cmps, data, number) =
        (ref (Array.copy !cmps), ref (TreeMap.fold TreeMap.add !data TreeMap.empty_def), ref !number)

    let head_copy (cmps, _, _) =
		let copy = make () in
    	Array.iter (fun s -> let _ = add_cmp (TreeSet.get_compare s) copy in ()) !cmps;
    	copy

    let size (_, _, number) =
        !number

    let has_node i (_, data, _) =
        TreeMap.mem i !data

    let has_edge i j (_, data, _) =
    	try
    		let (_, _, fwd) = TreeMap.find i !data in
    		TreeSet.mem j fwd
    	with Not_found ->
    		false

    let get_node_data v (_, data, _) =
    	let (d, _, _) = TreeMap.find v !data in d

    let set_node_data v d (_, data, _) =
    	let (_, bwd, fwd) = TreeMap.find v !data in
    	data := TreeMap.add v (d, bwd, fwd) !data

    let get_node_pred v (_, data, _) =
    	let (_, p, _) = TreeMap.find v !data in p

    let get_node_succ v (_, data, _) =
    	let (_, _, s) = TreeMap.find v !data in s

    let add_node v d (cmps, data, number) =
    	if not (has_node v (cmps, data, number)) then (
    		incr number;
    		data := TreeMap.add v (d, TreeSet.empty compare, TreeSet.empty compare) !data;
    		for i = 0 to Array.length !cmps - 1 do
    			(!cmps).(i) <- TreeSet.add v (!cmps).(i)
    		done
    	)

    let del_node v (cmps, data, number) =
    	if has_node v (cmps, data, number) then (
    		decr number;
    		let (_, _, vfwd) = TreeMap.find v !data in
    		TreeSet.iter (fun w ->
    			let (d, wbwd, wfwd) = TreeMap.find w !data in
		    	data := TreeMap.add w (d, TreeSet.remove v wbwd, wfwd) !data;
    		) vfwd;
    		let (_, vbwd, _) = TreeMap.find v !data in
    		TreeSet.iter (fun w ->
    			let (d, wbwd, wfwd) = TreeMap.find w !data in
		    	data := TreeMap.add w (d, wbwd, TreeSet.remove v wfwd) !data;
    		) vbwd;
    		data := TreeMap.remove v !data;
    		for i = 0 to Array.length !cmps - 1 do
    			(!cmps).(i) <- TreeSet.remove v (!cmps).(i)
    		done
    	)

    let add_edge v w (cmps, data, number) =
    	let (vdat, vbwd, vfwd) = TreeMap.find v !data in
    	data := TreeMap.add v (vdat, vbwd, TreeSet.add w vfwd) !data;
    	let (wdat, wbwd, wfwd) = TreeMap.find w !data in
    	data := TreeMap.add w (wdat, TreeSet.add v wbwd, wfwd) !data

    let del_edge v w (cmps, data, number) =
		if (has_node v (cmps, data, number)) && (has_node w (cmps, data, number)) then (
    		let (vdat, vbwd, vfwd) = TreeMap.find v !data in
    		data := TreeMap.add v (vdat, vbwd, TreeSet.remove w vfwd) !data;
    		let (wdat, wbwd, wfwd) = TreeMap.find w !data in
    		data := TreeMap.add w (wdat, TreeSet.remove v wbwd, wfwd) !data
    	)

    let iter f (_, data, _) =
    	TreeMap.iter f !data

    let iter_by f i (cmps, _, _) =
    	TreeSet.iter f (!cmps).(i)

    let sub_graph_by_edge_pred pred (cmps, data, number) =
    	let (cmps', data', number') = head_copy (cmps, data, number) in
    	TreeMap.iter (fun v (dat, _, fwd) ->
    		TreeSet.iter (fun w ->
    			if pred v w then (
    				add_node v dat (cmps', data', number');
    				add_node w (get_node_data w (cmps, data, number)) (cmps', data', number');
    				add_edge v w (cmps', data', number')
    			)
    		) fwd
    	) !data;
    	(cmps', data', number')

	let sub_graph_by_node_closure v next (cmps, data, number) =
		let (cmps', data', number') = head_copy (cmps, data, number) in
		let rec helper w =
			if not (has_node w (cmps', data', number')) then (
				add_node w (get_node_data w (cmps, data, number)) (cmps', data', number');
				next w helper
			)
		in
			helper v;
			iter (fun w _ ->
				let fwd = get_node_succ w (cmps, data, number) in
				TreeSet.iter (fun u ->
					if has_node u (cmps', data', number')
					then add_edge w u (cmps', data', number')
				) fwd
			) (cmps', data', number');
			(cmps', data', number')

	let depth_find v filt succ gr =
		let cache = ref (TreeSet.empty compare) in
		let rec process u =
			if TreeSet.mem u !cache then false else (
				cache := TreeSet.add u !cache;
				TreeSet.fold (fun w r -> r || (filt w && (succ w || process w))) (get_node_succ u gr) false
			)
		in
			process v
end



module Digraph = struct

	type 'a node' = {
		mutable content: 'a option;
		mutable fwd_edges: 'a nodes;
		mutable bwd_edges: 'a nodes;
	}
	and 'a node = ('a node') CompRef.compref
	and 'a nodes = ('a node) TreeSet.t
	
	type 'a digraph = {
		mutable nodes: 'a nodes;
		mutable node_count: int;
		mutable edge_count: int;
	}
	
	let compare_node = CompRef.compare
	
	let equal_node = CompRef.equal
	
	let empty_nodes _ =
		TreeSet.empty compare_node
		
	let create _ = {
		nodes = empty_nodes ();
		node_count = 0;
		edge_count = 0;
	}
	
	let node_count gr =
		gr.node_count
		
	let edge_count gr =
		gr.edge_count
		
	let add_empty_node gr =
		let node = CompRef.newref {
			content = None;
			fwd_edges = TreeSet.empty compare_node;
			bwd_edges = TreeSet.empty compare_node;
		} in
		gr.nodes <- TreeSet.add node gr.nodes;
		gr.node_count <- gr.node_count + 1;
		node
		
	let del_node gr node =
		if TreeSet.mem node gr.nodes then (
			let nodederef = CompRef.getref node in
			TreeSet.iter (fun node' ->
				let node' = CompRef.getref node' in
				node'.bwd_edges <- TreeSet.remove node node'.bwd_edges
			) nodederef.fwd_edges;
			TreeSet.iter (fun node' ->
				let node' = CompRef.getref node' in
				node'.fwd_edges <- TreeSet.remove node node'.fwd_edges
			) nodederef.bwd_edges;
			gr.nodes <- TreeSet.remove node gr.nodes;
			gr.node_count <- gr.node_count - 1;
			gr.edge_count <- gr.edge_count - TreeSet.cardinal nodederef.fwd_edges - TreeSet.cardinal nodederef.bwd_edges
		)

	let add_edge gr nodex nodey =
		let nodexderef = CompRef.getref nodex in
		if not (TreeSet.mem nodey nodexderef.fwd_edges) then (
			nodexderef.fwd_edges <- TreeSet.add nodey nodexderef.fwd_edges;
			let nodeyderef = CompRef.getref nodey in
			nodeyderef.bwd_edges <- TreeSet.add nodex nodeyderef.bwd_edges;
			gr.edge_count <- gr.edge_count + 1
		)
		
	let del_edge gr nodex nodey =
		let nodexderef = CompRef.getref nodex in
		if TreeSet.mem nodey nodexderef.fwd_edges then (
			let nodeyderef = CompRef.getref nodey in
			nodeyderef.bwd_edges <- TreeSet.remove nodex nodeyderef.bwd_edges;
			gr.edge_count <- gr.edge_count - 1
		)
	
	let get_content node =
		OptionUtils.get_some (CompRef.getref node).content

	let set_content node content =
		(CompRef.getref node).content <- Some content
	
	let get_fwd_edges node =
		(CompRef.getref node).fwd_edges
		
	let get_bwd_edges node =
		(CompRef.getref node).bwd_edges
		
	let get_nodes gr =
		gr.nodes

	let add_node gr content =
		let node = add_empty_node gr in
		set_content node content;
		node

end;;
