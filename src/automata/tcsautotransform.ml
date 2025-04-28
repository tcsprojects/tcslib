open Tcsbasedata;;
open Tcsautomata;;
open Tcsset;;
open Tcslist;;

module DBAtoNBA = struct

	let transform = DMA.asNMA

end;;


module DBAtoDPA = struct

	let transform = DBA.asDPA
	
	let transform2 = DBA.asDPA2

end;;

module DBAtoNPA = struct

	let transform auto = DMA.asNMA (DBA.asDPA auto)

end;;

module NBAtoNPA = struct

	let transform = NBA.asNPA

end;;

module DPAtoNPA = struct

	let transform = DMA.asNMA

end;;

module NPAtoNBA = struct

	type 'a state = ('a * int) option
	
	let size_upper_bound prios states = 1 + prios * states

	let transform (npa: ('a, 'b) NPA.t) evenprios =
		let (states, alphabet, initial, delta, omega) = (NMA.states npa,
		              NMA.alphabet npa, NMA.initial npa, NMA.delta npa, NPA.omega npa) in
		let fmt = Domain.format states in
		let comp = Domain.compare states in

		let states' =
			let comp' = Comparators.option_compare (Comparators.product comp compare) in
			let fmt' = Formators.option_format "DUMMY" "" (Formators.product fmt (fun p -> if p < 0 then "?" else string_of_int p)) in
			Domain.make comp' fmt'
		in
		
		let alphabet' = alphabet in
		
		let initial' =
			if TreeSet.is_empty evenprios then None else Some (initial, TreeSet.max_elt evenprios)
		in
		
		let delta' st a f = 
			match st with
				None -> ()
			|	Some (q, p) ->
					delta q a (fun q' ->
						let p' = omega q' in
						let prios' =
							if p' mod 2 = 0
							then TreeSet.filter (fun p'' -> p'' <= p) evenprios
							else TreeSet.filter (fun p'' -> (p' < p'') && (p'' <= p)) evenprios
						in
						let result = ref true in
						TreeSet.iter (fun p'' -> result := !result && f (Some (q', p''))) prios';
						!result
					)
		in
		
		let accept' = function
			None -> false
		|	Some (q, p) ->
				let p' = omega q in
				(p' mod 2 = 0) && (p' >= p)
		in
		
		NBA.build states' alphabet' initial' delta' accept'
end;;

module DPAtoNBA = struct

	type 'a state = 'a NPAtoNBA.state

	let size_upper_bound = NPAtoNBA.size_upper_bound

	let transform (dpa: ('a, 'b) DPA.t) = NPAtoNBA.transform (DMA.asNMA dpa)

end;;


(* Piterman *)
module NBAtoDPA = struct
	type 'a piterman = Node of int * ('a TreeSet.t) * (('a piterman) TreeSet.t)
	type 'a state = 'a piterman * int
	
	let transform (nba: ('a, 'b) NBA.t) size =
		let deref (Node (i,t,s)) = (i,t,s) in

		let (states, alphabet, initial, delta, accept) =
			(NMA.states nba, NMA.alphabet nba, NMA.initial nba, NMA.delta nba, NBA.accept nba) in
			
		let cmp = Domain.compare states in
		let fmt = Domain.format states in
		let inner_cmp x y = Comparators.product3 Comparators.default TreeSet.compare TreeSet.compare (deref x) (deref y) in		
		let rec inner_fmt x = Formators.product3 string_of_int (TreeSet.format fmt) (TreeSet.format inner_fmt) (deref x) in		
		let cmp' = Comparators.product inner_cmp Comparators.default in
		let fmt' = Formators.product inner_fmt string_of_int in
				
		let states' = Domain.make cmp' fmt' in
		
		let state_set_empty = TreeSet.empty cmp in
		let state_set_singleton = TreeSet.singleton cmp in
		let tree_set_empty = TreeSet.empty inner_cmp in
		let tree_set_singleton = TreeSet.singleton inner_cmp in
		
		let initial' = (Node (1, state_set_singleton initial, tree_set_empty), 1) in
		let omega' (_, q) = 2 * size - q in

		let delta' (tree, _) letter =
	        (* returns the maximal nodename *)
	        let rec maxNode (Node (n, _, children)) = TreeSet.fold (fun c -> max (maxNode c)) children n in
	        (* returns all nodenames *)
	        let getNodenames node =
	        	let rec collect (Node (n, _, children)) names =
	        		TreeSet.fold collect children (TreeSet.add n names)
	        	in
	        		collect node TreeSet.empty_def	
			in
	        (* -- I -- subset-construction *)
	        let rec subsetDelta (Node (n, states, children)) =
            	Node (n, TreeSet.fold (fun el -> Iterators.fold (delta el letter) TreeSet.add) states state_set_empty, TreeSet.map subsetDelta children) in
	        (* -- II -- split final states *)
	        let splitFinalStates minFreeName tree =
	          let mfn = ref minFreeName in
	          let rec splitFStates = function Node (n, states, children) ->
                 let fStates = TreeSet.filter accept states in
                 if TreeSet.is_empty fStates
                 then Node (n, states, TreeSet.map splitFStates children)
                 else (
                       let childrenDone = TreeSet.map splitFStates children in
                       let newChild = Node (!mfn, fStates, tree_set_empty) in
                       mfn := !mfn + 1;
                       Node (n, states, TreeSet.add newChild childrenDone)
                 )
              in
	            splitFStates tree in
		  (* -- III. -- "horizontal" merge *)
	      let rec hMerge = function Node (n, states, children) ->
	         (* merge siblings:
	                - union = union of labels of all older siblings
	                - newLabel = label without union
	            *)
	         let mergeSiblings siblings =
	                (* union of the states of all older nodes *)
	                let unionOlderSiblings n' node_set =
	                	TreeSet.fold (fun (Node (n, states, _)) s -> if n < n' then TreeSet.union s states else s) node_set state_set_empty
	                in
	                (* removes certain states from the labels of all descendents *)
	                let rec removeNodes toRemove (Node (n, states, children)) =
	                	Node (n, TreeSet.diff states toRemove, TreeSet.map (removeNodes toRemove) children)
	                in
	                TreeSet.map (fun (Node (n, states, children)) -> removeNodes (unionOlderSiblings n siblings) (Node (n, states, children))) siblings
	         in
                   Node (n, states, TreeSet.map hMerge (mergeSiblings children)) in
	      (* -- IV. -- vertical merge *)
	      let rec vMerge (Node (n, states, children)) =
	         (* union of all states of the nodes *)
	         let unionOfStates children = TreeSet.fold (fun (Node (_, states, _)) -> TreeSet.union states) children state_set_empty in
	         if TreeSet.is_empty children
	         then (size + 1, Node (n, states, tree_set_empty))
	         else if TreeSet.equal states (unionOfStates children)
	         then (n, Node (n, states, tree_set_empty))
			 else let (m, children_done) = TreeSet.fold (fun c (s, t) -> let (i, x) = vMerge c in (min s i, TreeSet.add x t)) children (size + 1, tree_set_empty) in
			      (m, Node (n, states, children_done))
		 in
	      (* -- V. -- remove empty nodes *)
	        let rec removeEmptyNodes nodes =
	        	TreeSet.fold (fun (Node (n, states, children)) (m, t) ->
	        		if TreeSet.is_empty states
	        		then (min n m, t)
	        		else let (n'', childrenDone) = removeEmptyNodes children in
	        		     (min n'' m, TreeSet.add (Node (n, states, childrenDone)) t)
	        	) nodes (size + 1, tree_set_empty)
			in
	        (* -- VI. -- reorganize nodenames in the tree *)
	        let rec reorganize removedNodes =
	            (* nodes in removedNodes which are less than n *)
	            let empty n = TreeSet.filter (fun x -> x < n) removedNodes in
	            function
	                Node (n, states, children) ->
	                    let childrenDone = TreeSet.map (reorganize removedNodes) children in
	                    Node (n - (TreeSet.cardinal (empty n)), states, childrenDone) in
	                    
	        let tree1 = subsetDelta tree in
	        let tree2 = splitFinalStates ((maxNode tree1)+1) tree1 in
	        let (f, tree3) = vMerge (hMerge tree2) in
	        let (e, tree4List) = removeEmptyNodes (tree_set_singleton tree3) in
	        let tree4 = TreeSet.max_elt tree4List in
	        let nodeNamesOfTree2 = getNodenames tree2 in
	        let nodeNamesOfTree4 = getNodenames tree4 in
	        let removedNodes = TreeSet.diff nodeNamesOfTree2 nodeNamesOfTree4 in
	        let tree5 = reorganize removedNodes tree4 in
	        let p = if e <= f then 2 * e - 3 else 2 * f - 2 in
	        (tree5, p)
		in
			
		DPA.build states' alphabet initial' delta' omega'
end


module NPAtoDPA = struct
	type 'a state = ('a NPAtoNBA.state) NBAtoDPA.state
	
	let transform (npa: ('a, 'b) NPA.t) even_prios state_size =
		NBAtoDPA.transform (NPAtoNBA.transform npa even_prios) (NPAtoNBA.size_upper_bound (TreeSet.cardinal even_prios) state_size)
end



(* Miyano-Hayshi *)
module NcoBAtoComplementDBA = struct

	type 'a state = 'a TreeSet.t * 'a TreeSet.t

	let transform (nba: ('a, 'b) NBA.t) =
	
		let (states, alphabet, initial, delta, accept) = (NMA.states nba,
		              NMA.alphabet nba, NMA.initial nba, NMA.delta nba, NBA.accept nba) in
		
		let cmp = Domain.compare states in
		let fmt = Domain.format states in
	
		let states' =
			let powercmp = TreeSet.compare in
			let powerfmt = TreeSet.format fmt in
			Domain.make (Comparators.product powercmp powercmp) (Formators.product powerfmt powerfmt)
		in
		
		let initial' =
			(TreeSet.singleton cmp initial, if accept initial then TreeSet.empty cmp else TreeSet.singleton cmp initial)
		in
		
		let delta' (e,o) r =
			let e' = ref (TreeSet.empty cmp) in
			TreeSet.iter (fun q ->	Iterators.iter (delta q r) (fun p -> e' := TreeSet.add p !e')) e;
			let o' = ref (TreeSet.empty cmp) in
			if TreeSet.is_empty o
			then o' := TreeSet.filter (fun q -> not (accept q)) !e'
			else TreeSet.iter (fun q -> Iterators.iter (delta q r) (fun p -> if not (accept p) then o' := TreeSet.add p !o')) o;
			(!e', !o')
		in
		
		let accept' s = TreeSet.is_empty (snd s) in
	
		DBA.build states' alphabet initial' delta' accept'

end;;


module GoodForGamesNBAtoNPA = struct

	type 'a state = ('a TreeSet.t * 'a TreeSet.t) list * int
	
	let transform (nba: ('a, 'b) NBA.t) state_size =
	
		let (states, alphabet, initial, delta, accept) = (NMA.states nba,
		              NMA.alphabet nba, NMA.initial nba, NMA.delta nba, NBA.accept nba) in
		
		let cmp = Domain.compare states in
		let fmt = Domain.format states in
		
		let empty = TreeSet.empty cmp in
		let singleton = TreeSet.singleton cmp in
		
		let cmp' = Comparators.product (ListUtils.compare_lists (Comparators.product TreeSet.compare TreeSet.compare)) Comparators.default in
		let fmt' = Formators.product (ListUtils.format (Formators.product (TreeSet.format fmt) (TreeSet.format fmt))) string_of_int in
		let states' = Domain.make cmp' fmt' in
		
		let initial' = 
			let f = accept initial in
			([(singleton initial, (if f then singleton initial else empty))], (if f then 0 else 1))
		in
		
		let delta' ((state, _): 'a state) letter =
			let em = empty in
			let finals a = TreeSet.filter accept a in
			let deltas a = TreeSet.fold (fun el acc -> Iterators.fold (delta el letter) TreeSet.add acc) a em in
			let fold_subs f ps b = TreeSet.fold_subsets f ps b in
			let fold_non_empty_subs f ps b = fold_subs (fun sub c -> if TreeSet.is_empty sub then c else f sub c) ps b in
			
			let f a acc' = fold_subs (fun sub acc -> (a,sub)::acc) (finals a) acc' in
			let f_ a = f a [] in
			let f' a = fold_non_empty_subs f a [] in
			let g a b acc' = fold_subs (fun sub acc -> (a,sub)::acc) (TreeSet.union (finals a) (TreeSet.inter a b)) acc' in
			let g_ a b = g a b [] in
			let g' a b = fold_non_empty_subs (fun a' -> g a' b) a [] in
			let h a acc' = fold_subs (fun sub acc -> (a,sub)::acc) a acc' in
			let h' a = fold_non_empty_subs h a [] in
			let k (a,b) = if TreeSet.cardinal a = TreeSet.cardinal b then f_ (deltas a) else g_ (deltas a) (deltas b) in
			let k' (a,b) = if TreeSet.cardinal a = TreeSet.cardinal b then f' (deltas a) else g' (deltas a) (deltas b) in 
			
			let attach head_list attachees =
				ListUtils.filter_map (fun (a,b) ->
					if List.for_all (fun (c,d) -> TreeSet.cardinal (TreeSet.inter a c) = 0 || TreeSet.subset a d) head_list
					then Some (head_list@[(a,b)])
					else None
				) attachees
			in
			
			let eta st =
				let st = Array.of_list st in
				let n = Array.length st in
				let a = deltas (fst st.(0)) in
				let m = TreeSet.cardinal a in
				let eta' m acc =
					let arr = Array.init m (fun i ->
						if i = 0 then k st.(0)
						else if i < n then k' st.(i)
						else h' a
					) in
					let l = ref (List.map (fun x -> [x]) arr.(0)) in
					for i = 1 to m - 1 do
						l := List.flatten (List.map (fun x -> attach x arr.(i)) !l)
					done;
					!l@acc
				in
				let rec hlp i acc = if i > m then acc else hlp (i+1) (eta' i acc) in
				hlp 1 []
			in
			
			let prio st =
				let fnd = ref false in
				let ind = ref 0 in
				let st = ref st in
				while (!st <> []) && not !fnd do
					let (a,b) = List.hd !st in
					st := List.tl !st;
					fnd := TreeSet.cardinal a = TreeSet.cardinal b;
					incr ind
				done;
				if !fnd then 2 * !ind - 2
				else 2 * !ind - 1
			in
			
			Iterators.of_list (List.map (fun st -> (st, prio st)) (eta state))				
		in
		
		let omega' q = 2 * state_size - snd q in
	
		NPA.build states' alphabet initial' delta' omega'	
end;;
