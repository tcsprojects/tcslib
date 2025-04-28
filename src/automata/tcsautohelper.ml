open Tcsbasedata;;
open Tcslist;;
open Tcsautomata;;
open Tcstiming;;
open Tcscache;;

module AlphabetCache = struct

    type 'a t = int Alphabet.alphabet * 'a IntTypeCache.t
    
    let make alpha x =
    	let cmp = Domain.compare alpha in
    	let fmt = Domain.format alpha in
    	let cache = IntTypeCache.make cmp x in
    	let cmp' = Comparators.default in
    	let fmt' y = fmt (IntTypeCache.decode cache y) in
    	let alpha' = Domain.make cmp' fmt' in
    	(alpha', cache)
    	
    let alphabet = fst
    
    let encode (_, x) = IntTypeCache.encode x

    let decode (_, x) = IntTypeCache.decode x
    
    let size (_, x) = IntTypeCache.size x
    
    let transform_nma nma (alpha, cache) =
    	NMA.build (NMA.states nma) alpha (NMA.initial nma) (fun x y -> NMA.delta nma x (IntTypeCache.decode cache y)) (NMA.accept nma)

    let transform_dma dma (alpha, cache) =
    	DMA.build (DMA.states dma) alpha (DMA.initial dma) (fun x y -> DMA.delta dma x (IntTypeCache.decode cache y)) (DMA.accept dma)
    	
    let iterate x iter =
    	Iterators.map (encode x) iter
  end
  

module NMAStateCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'c) NMA.t * (int, 'b, 'c) NMA.t * 'a IntTypeCache.t
    
    let make nma =
    	let states = NMA.states nma in
    	let initial = NMA.initial nma in
    	let cmp = Domain.compare states in
    	let fmt = Domain.format states in
    	let cache = IntTypeCache.make cmp initial in
    	let cmp' = Comparators.default in
    	let fmt' y = fmt (IntTypeCache.decode cache y) in
    	let states' = Domain.make cmp' fmt' in
    	(nma,
    	 NMA.build states' (NMA.alphabet nma) (IntTypeCache.encode cache initial)
    	           (fun x y -> Iterators.map (IntTypeCache.encode cache) (NMA.delta nma (IntTypeCache.decode cache x) y))
    	           (NMA.accept nma),
    	 cache)
    	 
    let automaton = Triple.snd

    let encode x = IntTypeCache.encode (Triple.trd x)

    let decode x = IntTypeCache.decode (Triple.trd x)
    
    let iterate x = IntTypeCache.iterate (Triple.trd x)
    
    let state_size x = IntTypeCache.size (Triple.trd x)
    
    let build x delta_image = IntTypeCache.build_recursively (Triple.trd x) delta_image
    
    let build_by_alphabet (a, _, x) alphabet_iterator =
    	IntTypeCache.build_recursively x (fun q -> Iterators.mapped_product alphabet_iterator (NMA.delta a q))


    type ('a, 'b, 'c) t2 = ('a, 'b, 'a -> 'c) NMA.t * (int, 'b, int -> 'c) NMA.t * 'a IntTypeCache.t

    let make2 nma =
    	let states = NMA.states nma in
    	let initial = NMA.initial nma in
    	let cmp = Domain.compare states in
    	let fmt = Domain.format states in
    	let cache = IntTypeCache.make cmp initial in
    	let cmp' = Comparators.default in
    	let fmt' y = fmt (IntTypeCache.decode cache y) in
    	let states' = Domain.make cmp' fmt' in
    	(nma,
    	 NMA.build states' (NMA.alphabet nma) (IntTypeCache.encode cache initial)
    	           (fun x y -> Iterators.map (IntTypeCache.encode cache) (NMA.delta nma (IntTypeCache.decode cache x) y))
    	           (fun x -> NMA.accept nma (IntTypeCache.decode cache x)),
    	 cache)

    let automaton2 = Triple.snd

    let encode2 x = IntTypeCache.encode (Triple.trd x)

    let decode2 x = IntTypeCache.decode (Triple.trd x)
    
    let iterate2 x = IntTypeCache.iterate (Triple.trd x)

    let state_size2 x = IntTypeCache.size (Triple.trd x)

    let build2 x delta_image = IntTypeCache.build_recursively (Triple.trd x) delta_image

    let build2_by_alphabet (a, _, x) alphabet_iterator =
    	IntTypeCache.build_recursively x (fun q -> Iterators.mapped_product alphabet_iterator (NMA.delta a q))

  end
  
  
module NMADeltaCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'c) NMA.t * ('a, 'b, 'a list) FunctionCache.t2

    let make nma =
    	let states = NMA.states nma in
    	let alphabet = NMA.alphabet nma in
    	let delta' = fun x y -> Iterators.to_list (NMA.delta nma x y) in
    	let cache = FunctionCache.cache_function2_make delta' (Domain.compare states) (Domain.compare alphabet) in
    	let delta'' = fun x y -> Iterators.of_list (FunctionCache.cache_function2_get cache x y) in
    	let auto = NMA.build states alphabet (NMA.initial nma) delta'' (NMA.accept nma) in
    	(auto, cache)    
    
    let automaton = fst

    let edge_size (_, cache) = FunctionCache.cache_function2_size cache
  end


module NMAAcceptCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'a -> 'c) NMA.t * ('a, 'c) FunctionCache.t
    
    let make nma =
    	let states = NMA.states nma in
    	let cache = FunctionCache.cache_function_make (NMA.accept nma) (Domain.compare states) in
    	let auto = NMA.build states (NMA.alphabet nma) (NMA.initial nma) (NMA.delta nma) (FunctionCache.cache_function_get cache) in
    	(auto, cache)    

    let automaton = fst
  end


module NMATiming = struct

	let delta_timing nma timingobj =
		NMA.build (NMA.states nma) (NMA.alphabet nma) (NMA.initial nma)
		          (fun s -> SimpleTiming.timed_function timingobj ((NMA.delta nma) s))
		          (NMA.accept nma)

	let accept_timing nma timingobj =
		NMA.build (NMA.states nma) (NMA.alphabet nma) (NMA.initial nma)
		          (NMA.delta nma)
		          (SimpleTiming.timed_function timingobj (NMA.accept nma))

	let full_timing nma timingobj =
		NMA.build (NMA.states nma) (NMA.alphabet nma) (NMA.initial nma)
		          (fun s -> SimpleTiming.timed_function timingobj ((NMA.delta nma) s))
		          (SimpleTiming.timed_function timingobj (NMA.accept nma))

end;;


module NMAFormatter = struct

	let header_to_string auto =
		"automaton \"" ^ auto ^ "\";"
		
	let alphabet_to_string alphabet_iter alphabet_fmt =
		let fmt a = string_of_int a ^ " \"" ^ alphabet_fmt a ^ "\";" in
		Iterators.fold alphabet_iter (fun a s -> s ^ "\n" ^ fmt a) "alphabet;"

	let states_to_string state_iter state_fmt omega =
		let fmt a = string_of_int a ^ " " ^ string_of_int (omega a) ^ " \"" ^ state_fmt a ^ "\";" in
		Iterators.fold state_iter (fun a s -> s ^ "\n" ^ fmt a) "states;"
		
	let initial_to_string i =
		"initial " ^ string_of_int i ^ ";"

	let delta_to_string delta state_iter alphabet_iter =
		Iterators.fold state_iter (fun state ->
			Iterators.fold alphabet_iter (fun symbol acc ->
				let l = Iterators.to_list (delta state symbol) in
				if l = [] then acc
				else 
					acc ^ "\n" ^
					string_of_int state ^ " " ^ string_of_int symbol ^ " " ^
					ListUtils.custom_format string_of_int "" "" "," l ^ ";"
			)
		) "transitions;"
		
		
	let npa_to_string2 auto state_iter alphabet_iter =
		header_to_string "npa" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (NMA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (NMA.states auto)) (NMA.accept auto) ^ "\n" ^ 
		initial_to_string (NMA.initial auto) ^ "\n" ^ 
		delta_to_string (NMA.delta auto) state_iter alphabet_iter ^ "\n"

	let nba_to_string2 auto state_iter alphabet_iter =
		header_to_string "nba" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (NMA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (NMA.states auto)) (NMA.accept (NBA.asNPA auto)) ^ "\n" ^ 
		initial_to_string (NMA.initial auto) ^ "\n" ^ 
		delta_to_string (NMA.delta auto) state_iter alphabet_iter ^ "\n"

	let npa_to_string auto alphabet_iter =
		let alphabet_cache = AlphabetCache.make (NMA.alphabet auto) (Iterators.first alphabet_iter) in
		let auto' = AlphabetCache.transform_nma auto alphabet_cache in
		let states_cache = NMAStateCache.make2 auto' in
		let alphabet_iter' = AlphabetCache.iterate alphabet_cache alphabet_iter in
		NMAStateCache.build2_by_alphabet states_cache alphabet_iter';
		let state_iter = Iterators.map fst (NMAStateCache.iterate states_cache) in
		let auto'' = NMAStateCache.automaton2 states_cache in
		npa_to_string2 auto'' state_iter alphabet_iter'
	
	let nba_to_string auto alphabet_iter =
		let alphabet_cache = AlphabetCache.make (NMA.alphabet auto) (Iterators.first alphabet_iter) in
		let auto' = AlphabetCache.transform_nma auto alphabet_cache in
		let states_cache = NMAStateCache.make2 auto' in
		let alphabet_iter' = AlphabetCache.iterate alphabet_cache alphabet_iter in
		NMAStateCache.build2_by_alphabet states_cache alphabet_iter';
		let state_iter = Iterators.map fst (NMAStateCache.iterate states_cache) in
		let auto'' = NMAStateCache.automaton2 states_cache in
		nba_to_string2 auto'' state_iter alphabet_iter'

end;;



module DMAStateCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'c) DMA.t * (int, 'b, 'c) DMA.t * 'a IntTypeCache.t
    
    let make dma =
    	let states = DMA.states dma in
    	let initial = DMA.initial dma in
    	let cmp = Domain.compare states in
    	let fmt = Domain.format states in
    	let cache = IntTypeCache.make cmp initial in
    	let cmp' = Comparators.default in
    	let fmt' y = fmt (IntTypeCache.decode cache y) in
    	let states' = Domain.make cmp' fmt' in
    	(dma,
    	 DMA.build states' (DMA.alphabet dma) (IntTypeCache.encode cache initial)
    	           (fun x y -> IntTypeCache.encode cache (DMA.delta dma (IntTypeCache.decode cache x) y))
    	           (DMA.accept dma),
    	 cache)
    	 
    let automaton = Triple.snd

    let encode (_, _, x) = IntTypeCache.encode x

    let decode (_, _, x) = IntTypeCache.decode x
    
    let iterate x = IntTypeCache.iterate (Triple.trd x)

    let state_size (_, _, x) = IntTypeCache.size x

    let build x delta_image = IntTypeCache.build_recursively (Triple.trd x) delta_image
    
    let build_by_alphabet (a, _, x) alphabet_iterator =
    	IntTypeCache.build_recursively x (fun q -> Iterators.map (DMA.delta a q) alphabet_iterator)

    type ('a, 'b, 'c) t2 = ('a, 'b, 'a -> 'c) DMA.t * (int, 'b, int -> 'c) DMA.t * 'a IntTypeCache.t

    let make2 dma =
    	let states = DMA.states dma in
    	let initial = DMA.initial dma in
    	let cmp = Domain.compare states in
    	let fmt = Domain.format states in
    	let cache = IntTypeCache.make cmp initial in
    	let cmp' = Comparators.default in
    	let fmt' y = fmt (IntTypeCache.decode cache y) in
    	let states' = Domain.make cmp' fmt' in
    	(dma,
    	 DMA.build states' (DMA.alphabet dma) (IntTypeCache.encode cache initial)
    	           (fun x y -> IntTypeCache.encode cache (DMA.delta dma (IntTypeCache.decode cache x) y))
    	           (fun x -> DMA.accept dma (IntTypeCache.decode cache x)),
    	 cache)

    let automaton2 = Triple.snd

    let encode2 (_, _, x) = IntTypeCache.encode x

    let decode2 (_, _, x) = IntTypeCache.decode x
    
    let iterate2 x = IntTypeCache.iterate (Triple.trd x)

    let state_size2 (_, _, x) = IntTypeCache.size x

    let build2 x delta_image = IntTypeCache.build_recursively (Triple.trd x) delta_image

    let build2_by_alphabet (a, _, x) alphabet_iterator =
    	IntTypeCache.build_recursively x (fun q -> Iterators.map (DMA.delta a q) alphabet_iterator)

  end
  
  
module DMADeltaCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'c) DMA.t * ('a, 'b, 'a) FunctionCache.t2

    let make dma =
    	let states = DMA.states dma in
    	let alphabet = DMA.alphabet dma in
    	let cache = FunctionCache.cache_function2_make (DMA.delta dma) (Domain.compare states) (Domain.compare alphabet) in
    	let auto = DMA.build states alphabet (DMA.initial dma) (FunctionCache.cache_function2_get cache) (DMA.accept dma) in
    	(auto, cache)    
    
    let automaton = fst

    let edge_size (_, cache) = FunctionCache.cache_function2_size cache
  end


module DMAAcceptCache = struct

    type ('a, 'b, 'c) t = ('a, 'b, 'a -> 'c) DMA.t * ('a, 'c) FunctionCache.t
    
    let make dma =
    	let states = DMA.states dma in
    	let cache = FunctionCache.cache_function_make (DMA.accept dma) (Domain.compare states) in
    	let auto = DMA.build states (DMA.alphabet dma) (DMA.initial dma) (DMA.delta dma) (FunctionCache.cache_function_get cache) in
    	(auto, cache)    

    let automaton = fst
  end


module DMATiming = struct

	let delta_timing dma timingobj =
		DMA.build (DMA.states dma) (DMA.alphabet dma) (DMA.initial dma)
		          (fun s -> SimpleTiming.timed_function timingobj ((DMA.delta dma) s))
		          (DMA.accept dma)

	let accept_timing dma timingobj =
		DMA.build (DMA.states dma) (DMA.alphabet dma) (DMA.initial dma)
		          (DMA.delta dma)
		          (SimpleTiming.timed_function timingobj (DMA.accept dma))

	let full_timing dma timingobj =
		DMA.build (DMA.states dma) (DMA.alphabet dma) (DMA.initial dma)
		          (fun s -> SimpleTiming.timed_function timingobj ((DMA.delta dma) s))
		          (SimpleTiming.timed_function timingobj (DMA.accept dma))

end;;



module DMAFormatter = struct

	let header_to_string auto =
		"automaton \"" ^ auto ^ "\";"
		
	let alphabet_to_string alphabet_iter alphabet_fmt =
		let fmt a = string_of_int a ^ " \"" ^ alphabet_fmt a ^ "\";" in
		Iterators.fold alphabet_iter (fun a s -> s ^ "\n" ^ fmt a) "alphabet;"

	let states_to_string state_iter state_fmt omega =
		let fmt a = string_of_int a ^ " " ^ string_of_int (omega a) ^ " \"" ^ state_fmt a ^ "\";" in
		Iterators.fold state_iter (fun a s -> s ^ "\n" ^ fmt a) "states;"
		
	let initial_to_string i =
		"initial " ^ string_of_int i ^ ";"

	let delta_to_string delta state_iter alphabet_iter =
		Iterators.fold state_iter (fun state ->
			Iterators.fold alphabet_iter (fun symbol acc ->
				acc ^ "\n" ^
				string_of_int state ^ " " ^ string_of_int symbol ^ " " ^
				string_of_int (delta state symbol) ^ ";"
			)
		) "transitions;"
		
		
	let dpa_to_string2 auto state_iter alphabet_iter =
		header_to_string "dpa" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (DMA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (DMA.states auto)) (DMA.accept auto) ^ "\n" ^ 
		initial_to_string (DMA.initial auto) ^ "\n" ^ 
		delta_to_string (DMA.delta auto) state_iter alphabet_iter ^ "\n"

	let dba_to_string2 auto state_iter alphabet_iter =
		header_to_string "dba" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (DMA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (DMA.states auto)) (DMA.accept (DBA.asDPA auto)) ^ "\n" ^ 
		initial_to_string (DMA.initial auto) ^ "\n" ^ 
		delta_to_string (DMA.delta auto) state_iter alphabet_iter ^ "\n"

	let dpa_to_string auto alphabet_iter =
		let alphabet_cache = AlphabetCache.make (DMA.alphabet auto) (Iterators.first alphabet_iter) in
		let auto' = AlphabetCache.transform_dma auto alphabet_cache in
		let states_cache = DMAStateCache.make2 auto' in
		let alphabet_iter' = AlphabetCache.iterate alphabet_cache alphabet_iter in
		DMAStateCache.build2_by_alphabet states_cache alphabet_iter';
		let state_iter = Iterators.map fst (DMAStateCache.iterate states_cache) in
		let auto'' = DMAStateCache.automaton2 states_cache in
		dpa_to_string2 auto'' state_iter alphabet_iter'
	
	let dba_to_string auto alphabet_iter =
		let alphabet_cache = AlphabetCache.make (DMA.alphabet auto) (Iterators.first alphabet_iter) in
		let auto' = AlphabetCache.transform_dma auto alphabet_cache in
		let states_cache = DMAStateCache.make2 auto' in
		let alphabet_iter' = AlphabetCache.iterate alphabet_cache alphabet_iter in
		DMAStateCache.build2_by_alphabet states_cache alphabet_iter';
		let state_iter = Iterators.map fst (DMAStateCache.iterate states_cache) in
		let auto'' = DMAStateCache.automaton2 states_cache in
		dba_to_string2 auto'' state_iter alphabet_iter'

end;;



module Priorities = struct

	let max_by cmp a b =
		let c = cmp a b in
		if c < 0 then b else a
		
	let relevance_compare = compare
	
	let relevance_max = max_by relevance_compare
	
	let reward p = if p mod 2 = 0 then p else -p
	
	let reward_compare a b = compare (reward a) (reward b)
	
	let reward_max = max_by reward_compare

end;;



module NMVPAFormatter = struct

	let header_to_string auto =
		"automaton \"" ^ auto ^ "\";"
		
	let alphabet_to_string alphabet_iter alphabet_fmt =
		let h = function NMVPA.Internal a -> string_of_int a | NMVPA.Push a -> string_of_int a ^ " <" | NMVPA.Pop a -> string_of_int a ^ " >" in
		let fmt a = h a ^ " \"" ^ alphabet_fmt a ^ "\";" in
		Iterators.fold alphabet_iter (fun a s -> s ^ "\n" ^ fmt a) "alphabet;"

	let states_to_string state_iter state_fmt omega =
		let fmt a = string_of_int a ^ " " ^ string_of_int (omega a) ^ " \"" ^ state_fmt a ^ "\";" in
		Iterators.fold state_iter (fun a s -> s ^ "\n" ^ fmt a) "states;"
		
	let stack_to_string stack_iter stack_fmt =
		let fmt a = string_of_int a ^ " \"" ^ stack_fmt a ^ "\";" in
		Iterators.fold stack_iter (fun a s -> s ^ "\n" ^ fmt a) "stack;"

	let initial_to_string i =
		"initial " ^ string_of_int i ^ ";"

	let delta_to_string delta_internal delta_push delta_pop state_iter alphabet_iter stack_iter =
		let h = function NMVPA.Internal i -> i | NMVPA.Push i -> i | NMVPA.Pop i -> i in
		let s = Iterators.fold state_iter (fun state ->
			Iterators.fold (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alphabet_iter) (fun symbol acc ->
				let l = Iterators.to_list (delta_internal state symbol) in
				if l = [] then acc
				else 
					acc ^ "\n" ^
					string_of_int state ^ " " ^ string_of_int (h symbol) ^ " " ^
					ListUtils.custom_format string_of_int "" "" "," l ^ ";"
			)
		) "transitions;" in
		let t = Iterators.fold state_iter (fun state ->
			Iterators.fold (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alphabet_iter) (fun symbol acc ->
				let l = Iterators.to_list (delta_push state symbol) in
				if l = [] then acc
				else 
					acc ^ "\n" ^
					string_of_int state ^ " " ^ string_of_int (h symbol) ^ " " ^
					ListUtils.custom_format (fun (a,b) -> "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")") "" "" "," l ^ ";"
			)
		) s in
		let r = Iterators.fold state_iter (fun state ->
			Iterators.fold (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alphabet_iter) (fun symbol ->
				Iterators.fold stack_iter (fun stack acc -> 
					let l = Iterators.to_list (delta_pop state symbol (Some stack)) in
					if l = [] then acc
					else 
						acc ^ "\n" ^
						string_of_int state ^ " " ^ string_of_int (h symbol) ^ " " ^ string_of_int stack ^ " " ^
						ListUtils.custom_format string_of_int "" "" "," l ^ ";"
				)
			)
		) t in
		r
		
		
	let npvpa_to_string2 auto state_iter alphabet_iter stack_iter =
		header_to_string "npvpa" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (NMVPA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (NMVPA.states auto)) (NMVPA.accept auto) ^ "\n" ^ 
		stack_to_string stack_iter (Domain.format (NMVPA.stack auto)) ^ "\n" ^ 
		initial_to_string (NMVPA.initial auto) ^ "\n" ^ 
		delta_to_string (NMVPA.delta_internal auto) (NMVPA.delta_push auto) (NMVPA.delta_pop auto) state_iter alphabet_iter stack_iter ^ "\n"

	let nbvpa_to_string2 auto state_iter alphabet_iter stack_iter=
		header_to_string "nbvpa" ^ "\n" ^
		alphabet_to_string alphabet_iter (Domain.format (NMVPA.alphabet auto))^ "\n" ^ 
		states_to_string state_iter (Domain.format (NMVPA.states auto)) (NMVPA.accept (NBVPA.asNPVPA auto)) ^ "\n" ^ 
		stack_to_string stack_iter (Domain.format (NMVPA.stack auto)) ^ "\n" ^ 
		initial_to_string (NMVPA.initial auto) ^ "\n" ^ 
		delta_to_string (NMVPA.delta_internal auto) (NMVPA.delta_push auto) (NMVPA.delta_pop auto) state_iter alphabet_iter stack_iter ^ "\n"

end;;