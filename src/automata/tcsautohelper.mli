open Tcsbasedata;;
open Tcsset;;
open Tcsautomata;;
open Tcstiming;;

module AlphabetCache :
  sig
    type 'a t
    val make: 'a Alphabet.alphabet -> 'a -> 'a t
    val alphabet: 'a t -> int Alphabet.alphabet
    val encode: 'a t -> 'a -> int
    val decode: 'a t -> int -> 'a
    val size: 'a t -> int
    val transform_nma: ('a, 'b, 'c) NMA.t -> 'b t -> ('a, int, 'c) NMA.t
    val transform_dma: ('a, 'b, 'c) DMA.t -> 'b t -> ('a, int, 'c) DMA.t
    val iterate: 'a t -> 'a Iterators.iterator -> int Iterators.iterator
  end
  
module NMAStateCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'c) NMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> (int, 'b, 'c) NMA.t
    val encode: ('a, 'b, 'c) t -> 'a -> int
    val decode: ('a, 'b, 'c) t -> int -> 'a
    val iterate: ('a, 'b, 'c) t -> (int * 'a) Iterators.iterator
    val state_size: ('a, 'b, 'c) t -> NMAFunctions.state_size
    val build: ('a, 'b, 'c) t -> 'a NMAFunctions.delta_image -> unit
    val build_by_alphabet: ('a, 'b, 'c) t -> 'b Iterators.iterator -> unit

    type ('a, 'b, 'c) t2
    
    val make2: ('a, 'b, 'a -> 'c) NMA.t -> ('a, 'b, 'c) t2
    val automaton2: ('a, 'b, 'c) t2 -> (int, 'b, int -> 'c) NMA.t
    val encode2: ('a, 'b, 'c) t2 -> 'a -> int
    val decode2: ('a, 'b, 'c) t2 -> int -> 'a
    val iterate2: ('a, 'b, 'c) t2 -> (int * 'a) Iterators.iterator
    val state_size2: ('a, 'b, 'c) t2 -> NMAFunctions.state_size
    val build2: ('a, 'b, 'c) t2 -> 'a NMAFunctions.delta_image -> unit
    val build2_by_alphabet: ('a, 'b, 'c) t2 -> 'b Iterators.iterator -> unit
  end
  
module NMADeltaCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'c) NMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> ('a, 'b, 'c) NMA.t
    val edge_size: ('a, 'b, 'c) t -> NMAFunctions.edge_size
  end
  
module NMAAcceptCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'a -> 'c) NMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> ('a, 'b, 'a -> 'c) NMA.t
  end
  
module NMATiming:
  sig
    val delta_timing: ('a, 'b, 'c) NMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'c) NMA.t
    val accept_timing: ('a, 'b, 'a -> 'c) NMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'a -> 'c) NMA.t
    val full_timing: ('a, 'b, 'a -> 'c) NMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'a -> 'c) NMA.t
  end
  
  
module NMAFormatter:
  sig
    val npa_to_string: ('a, 'b) NPA.t -> 'b Iterators.iterator -> string
    val nba_to_string: ('a, 'b) NBA.t -> 'b Iterators.iterator -> string
    val npa_to_string2: (int, int) NPA.t -> int Iterators.iterator -> int Iterators.iterator -> string
    val nba_to_string2: (int, int) NBA.t -> int Iterators.iterator -> int Iterators.iterator -> string
  end
  
  
module DMAStateCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'c) DMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> (int, 'b, 'c) DMA.t
    val encode: ('a, 'b, 'c) t -> 'a -> int
    val decode: ('a, 'b, 'c) t -> int -> 'a
    val iterate: ('a, 'b, 'c) t -> (int * 'a) Iterators.iterator
    val state_size: ('a, 'b, 'c) t -> NMAFunctions.state_size
    val build: ('a, 'b, 'c) t -> 'a NMAFunctions.delta_image -> unit
    val build_by_alphabet: ('a, 'b, 'c) t -> 'b Iterators.iterator -> unit

    type ('a, 'b, 'c) t2
    val make2: ('a, 'b, 'a -> 'c) DMA.t -> ('a, 'b, 'c) t2
    val automaton2: ('a, 'b, 'c) t2 -> (int, 'b, int -> 'c) DMA.t
    val encode2: ('a, 'b, 'c) t2 -> 'a -> int
    val decode2: ('a, 'b, 'c) t2 -> int -> 'a
    val iterate2: ('a, 'b, 'c) t2 -> (int * 'a) Iterators.iterator
    val state_size2: ('a, 'b, 'c) t2 -> NMAFunctions.state_size
    val build2: ('a, 'b, 'c) t2 -> 'a NMAFunctions.delta_image -> unit
    val build2_by_alphabet: ('a, 'b, 'c) t2 -> 'b Iterators.iterator -> unit
  end
  
module DMADeltaCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'c) DMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> ('a, 'b, 'c) DMA.t
    val edge_size: ('a, 'b, 'c) t -> NMAFunctions.edge_size
  end
  
module DMAAcceptCache :
  sig
    type ('a, 'b, 'c) t
    val make: ('a, 'b, 'a -> 'c) DMA.t -> ('a, 'b, 'c) t
    val automaton: ('a, 'b, 'c) t -> ('a, 'b, 'a -> 'c) DMA.t
  end
  
module DMATiming:
  sig
    val delta_timing: ('a, 'b, 'c) DMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'c) DMA.t
    val accept_timing: ('a, 'b, 'a -> 'c) DMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'a -> 'c) DMA.t
    val full_timing: ('a, 'b, 'a -> 'c) DMA.t -> SimpleTiming.timing_object -> ('a, 'b, 'a -> 'c) DMA.t
  end
  
  
module DMAFormatter:
  sig
    val dpa_to_string: ('a, 'b) DPA.t -> 'b Iterators.iterator -> string
    val dba_to_string: ('a, 'b) DBA.t -> 'b Iterators.iterator -> string
    val dpa_to_string2: (int, int) DPA.t -> int Iterators.iterator -> int Iterators.iterator -> string
    val dba_to_string2: (int, int) DBA.t -> int Iterators.iterator -> int Iterators.iterator -> string
  end
  
  
module Priorities :
  sig
    val max_by : (int -> int -> int) -> int -> int -> int
    val relevance_compare : int -> int -> int
    val relevance_max : int -> int -> int
    val reward : int -> int
    val reward_compare : int -> int -> int
    val reward_max : int -> int -> int
  end
  
  
module NMVPAFormatter:
  sig
    val npvpa_to_string2: (int, int, int) NPVPA.t -> int Iterators.iterator -> (int NMVPA.nested) Iterators.iterator -> int Iterators.iterator -> string
    val nbvpa_to_string2: (int, int, int) NBVPA.t -> int Iterators.iterator -> (int NMVPA.nested) Iterators.iterator -> int Iterators.iterator -> string
  end
  