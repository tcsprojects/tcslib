open Tcsset;;
open Tcsbasedata;;
open Tcslist;;

		
module Alphabet = struct

	type 'a alphabet = 'a Domain.t
	
	type 'a word = 'a list
	
    type 'a omega_word = 'a word * 'a word	
	
	let compare_words alph =
		ListUtils.compare_lists (Domain.compare alph)

	let empty_word _ = []
	
	let singleton_word x = [x]
	
	let attach_left_word x l = x::l
	
	let attach_right_word l x = l @ [x]
	
	let compose_words l l' = l @ l'
	
	let format_word alph =
		ListUtils.format_plain (Domain.format alph)
		
	let omega_simplify alph word =
		let a = Array.of_list word in
		let len = Array.length a in
		let prefix_check pfx =
			if len mod pfx = 0 then (
				let positive = ref true in
				Array.iteri (fun i x ->
					positive := !positive && (Domain.compare alph a.(i mod pfx) x = 0)
				) a;
				!positive
			)
			else false
		in
		let i = ref 0 in
		let searching = ref true in
		while (!i+1 < len) && !searching do
			incr i;
			searching := not (prefix_check !i);
		done;
		if !searching then word else (Array.to_list (Array.sub a 0 !i))
		
	let format_omega_word alph (prefix, cycle) =
		format_word alph prefix ^ "(" ^ format_word alph (omega_simplify alph cycle) ^ ")^omega"

end;;		


module NMA = struct

	type ('q, 'a, 'c) t = 'q Domain.t * (* state set *)
	                    'a Alphabet.alphabet * (* alphabet *)
						'q * (* initial state *)
						('q -> 'a -> 'q Iterators.iterator) * (* transition relation *)
						'c (* acceptance condition *)

	let build s a i d o = (s, a, i, d, o)
	
	let states (s, _, _, _, _) = s
	
	let alphabet (_, a, _, _, _) = a
	
	let initial (_, _, q, _, _) = q

	let delta (_, _, _, d, _) = d
	
	let accept (_, _, _, _, a) = a

end;;


module NMAFunctions = struct
	type 'a state_set = 'a TreeSet.t
	type state_size = int
	type edge_size = int
	type 'a delta_image = 'a -> 'a Iterators.iterator
	type 'a state_iterator = 'a Iterators.iterator
	type 'a alphabet_iterator = 'a Iterators.iterator
end  


module NPA = struct

	type ('q, 'a) t = ('q, 'a, ('q -> int)) NMA.t
	
	let build = NMA.build
	
	let omega = NMA.accept
	
end;;


module NPAFunctions = struct
    type priority_set = int TreeSet.t
    type even_priority_set = int TreeSet.t
    type odd_priority_set = int TreeSet.t
    type priority_size = int
    type even_priority_size = int
    type odd_priority_size = int
end;;


module NBA = struct

	type ('q, 'a) t = ('q, 'a, ('q -> bool)) NMA.t
	
	let build = NMA.build
	
	let accept = NMA.accept
	
	let asNPA auto =
		NPA.build (NMA.states auto) (NMA.alphabet auto) (NMA.initial auto)
		          (NMA.delta auto) (fun q -> if accept auto q then 2 else 1)
	
	let complementAcceptance auto =
		build (NMA.states auto) (NMA.alphabet auto) (NMA.initial auto)
		      (NMA.delta auto) (fun q -> not (accept auto q))
	
end;;


module DMA = struct

	type ('q, 'a, 'c) t = 'q Domain.t * (* state set *)
	                    'a Alphabet.alphabet * (* alphabet *)
						'q * (* initial state *)
						('q -> 'a -> 'q) * (* transition relation *)
						'c (* acceptance condition *)

	let build s a i d o = (s, a, i, d, o)
	
	let states (s, _, _, _, _) = s
	
	let alphabet (_, a, _, _, _) = a
	
	let initial (_, _, q, _, _) = q

	let delta (_, _, _, d, _) = d
	
	let accept (_, _, _, _, a) = a
	
	let asNMA (s, a, i, d, o) = NMA.build s a i (fun x y -> Iterators.singleton (d x y)) o

end;;

module DPA = struct

	type ('q, 'a) t = ('q, 'a, ('q -> int)) DMA.t
	
	let build = DMA.build
	
	let omega = DMA.accept
	
end;;

module DBA = struct

	type ('q, 'a) t = ('q, 'a, ('q -> bool)) DMA.t
	
	let build = NMA.build
	
	let accept = NMA.accept
	
	let asDPA2 auto for_true for_false =
		DPA.build (DMA.states auto) (DMA.alphabet auto) (DMA.initial auto)
		          (DMA.delta auto) (fun q -> if accept auto q then for_true else for_false)
	
	let asDPA auto = asDPA2 auto 2 1
end;;



module NMVPAFunctions = struct
	type 'a stack_iterator = 'a Iterators.iterator
end  

module NMVPA = struct

	type 'a nested = Internal of 'a | Push of 'a | Pop of 'a

	type ('q, 'a, 'c, 'd) t = 'q Domain.t * (* state set *)
	                    ('a nested) Alphabet.alphabet * (* alphabet *)
	                    'd Domain.t * (* stack set *)
						'q * (* initial state *)
						('q -> 'a nested -> 'q Iterators.iterator) * (* internal transition relation *)
						('q -> 'a nested -> ('q * 'd) Iterators.iterator) * (* push transition relation *)
						('q -> 'a nested -> 'd option -> 'q Iterators.iterator) * (* pop transition relation *)
						'c (* acceptance condition *)

	let build s a st i di dpu dpo o = (s, a, st, i, di, dpu, dpo, o)
	
	let states (s, _, _, _, _, _, _, _) = s
	
	let alphabet (_, a, _, _, _, _, _, _) = a
	
	let stack (_, _, s, _, _, _, _, _) = s

	let initial (_, _, _, q, _, _, _, _) = q

	let delta_internal (_, _, _, _, d, _, _, _) = d
	
	let delta_push (_, _, _, _, _, d, _, _) = d

	let delta_pop (_, _, _, _, _, _, d, _) = d

	let accept (_, _, _, _, _, _, _, a) = a

end;;



module NPVPA = struct

	type ('q, 'a, 'd) t = ('q, 'a, ('q -> int), 'd) NMVPA.t
	
	let build = NMVPA.build
	
	let omega = NMVPA.accept
	
end;;


module NBVPA = struct

	type ('q, 'a, 'd) t = ('q, 'a, ('q -> bool), 'd) NMVPA.t
	
	let build = NMVPA.build
	
	let accept = NMVPA.accept
	
	let asNPVPA auto =
		NPVPA.build (NMVPA.states auto) (NMVPA.alphabet auto) (NMVPA.stack auto) (NMVPA.initial auto)
		            (NMVPA.delta_internal auto) (NMVPA.delta_push auto) (NMVPA.delta_pop auto)
		            (fun q -> if accept auto q then 2 else 1)

	let byNPVPA auto map =
		build       (NMVPA.states auto) (NMVPA.alphabet auto) (NMVPA.stack auto) (NMVPA.initial auto)
		            (NMVPA.delta_internal auto) (NMVPA.delta_push auto) (NMVPA.delta_pop auto)
		            (fun q -> map (NPVPA.omega auto q))
					
	let complementAcceptance auto =
		build (NMVPA.states auto) (NMVPA.alphabet auto) (NMVPA.stack auto) (NMVPA.initial auto)
		      (NMVPA.delta_internal auto) (NMVPA.delta_push auto) (NMVPA.delta_pop auto) (fun q -> not (accept auto q))
	
end;;


module DMVPA = struct

	type 'a nested = 'a NMVPA.nested

	type ('q, 'a, 'c, 'd) t = 'q Domain.t * (* state set *)
	                    ('a nested) Alphabet.alphabet * (* alphabet *)
	                    'd Domain.t * (* stack set *)
						'q * (* initial state *)
						('q -> 'a nested -> 'q) * (* internal transition relation *)
						('q -> 'a nested -> ('q * 'd)) * (* push transition relation *)
						('q -> 'a nested -> 'd option -> 'q) * (* pop transition relation *)
						'c (* acceptance condition *)

	let build s a st i di dpu dpo o = (s, a, st, i, di, dpu, dpo, o)
	
	let states (s, _, _, _, _, _, _, _) = s
	
	let alphabet (_, a, _, _, _, _, _, _) = a
	
	let stack (_, _, s, _, _, _, _, _) = s

	let initial (_, _, _, q, _, _, _, _) = q

	let delta_internal (_, _, _, _, d, _, _, _) = d
	
	let delta_push (_, _, _, _, _, d, _, _) = d

	let delta_pop (_, _, _, _, _, _, d, _) = d

	let accept (_, _, _, _, _, _, _, a) = a

	let asNMVPA (s, a, st, i, di, dpu, dpo, o) = NMVPA.build s a st i (fun x y -> Iterators.singleton (di x y)) (fun x y -> Iterators.singleton (dpu x y)) (fun x y z -> Iterators.singleton (dpo x y z)) o

end;;


module DPVPA = struct

	type ('q, 'a, 'd) t = ('q, 'a, ('q -> int), 'd) DMVPA.t
	
	let build = NMVPA.build
	
	let omega = NMVPA.accept
	
end;;


module DBVPA = struct

	type ('q, 'a, 'd) t = ('q, 'a, ('q -> bool), 'd) DMVPA.t
	
	let build = NMVPA.build
	
	let accept = NMVPA.accept
	
	let asDPVPA2 auto for_true for_false =
		DPVPA.build (DMVPA.states auto) (DMVPA.alphabet auto) (DMVPA.stack auto) (DMVPA.initial auto)
		          (DMVPA.delta_internal auto) (DMVPA.delta_push auto) (DMVPA.delta_pop auto) (fun q -> if accept auto q then for_true else for_false)
	
	let asDPVPA auto = asDPVPA2 auto 2 1
	
end;;
