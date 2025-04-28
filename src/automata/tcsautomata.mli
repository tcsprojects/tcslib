open Tcsbasedata
open Tcsset

module Alphabet :
  sig
    type 'a alphabet = 'a Domain.t
    type 'a word = 'a list
    type 'a omega_word = 'a word * 'a word
	
    val compare_words :
      'a alphabet -> 'a word -> 'a word -> int
    val empty_word : unit -> 'a word
    val singleton_word : 'a -> 'a word
    val attach_left_word : 'a -> 'a word -> 'a word
    val attach_right_word : 'a word -> 'a -> 'a word
    val compose_words : 'a word -> 'a word -> 'a word
    val format_word : 'a alphabet -> 'a word -> string
	val omega_simplify: 'a alphabet -> 'a word -> 'a word
	val format_omega_word: 'a alphabet -> 'a omega_word -> string
  end
  
module NMA :
  sig
    type ('a, 'b, 'c) t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a Iterators.iterator)	-> 'c -> ('a, 'b, 'c) t
    val states : ('a, 'b, 'c) t -> 'a Domain.t
    val alphabet : ('a, 'b, 'c) t -> 'b Alphabet.alphabet
    val initial : ('a, 'b, 'c) t -> 'a
    val delta : ('a, 'b, 'c) t -> ('a -> 'b -> 'a Iterators.iterator)
    val accept : ('a, 'b, 'c) t -> 'c
  end
  
module NMAFunctions : sig
	type 'a state_set = 'a TreeSet.t
	type state_size = int
	type edge_size = int
	type 'a delta_image = 'a -> 'a Iterators.iterator
	type 'a state_iterator = 'a Iterators.iterator
	type 'a alphabet_iterator = 'a Iterators.iterator
end  

module NPA :
  sig
    type ('a, 'b) t = ('a, 'b, ('a -> int)) NMA.t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a Iterators.iterator)	-> ('a -> int)
				-> ('a, 'b) t
    val omega : ('a, 'b) t -> ('a -> int)
  end
  
module NPAFunctions : sig
    type priority_set = int TreeSet.t
    type even_priority_set = int TreeSet.t
    type odd_priority_set = int TreeSet.t
    type priority_size = int
    type even_priority_size = int
    type odd_priority_size = int
end
  
module NBA :
  sig
    type ('a, 'b) t = ('a, 'b, ('a -> bool)) NMA.t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a Iterators.iterator)	-> ('a -> bool)
				-> ('a, 'b) t
    val accept : ('a, 'b) t -> ('a -> bool)
	val asNPA: ('a, 'b) t -> ('a, 'b) NPA.t
	val complementAcceptance: ('a, 'b) t -> ('a, 'b) t
  end

module DMA :
  sig
    type ('a, 'b, 'c) t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a) -> 'c -> ('a, 'b, 'c) t
    val states : ('a, 'b, 'c) t -> 'a Domain.t
    val alphabet : ('a, 'b, 'c) t -> 'b Alphabet.alphabet
    val initial : ('a, 'b, 'c) t -> 'a
    val delta : ('a, 'b, 'c) t -> ('a -> 'b -> 'a)
    val accept : ('a, 'b, 'c) t -> 'c
    val asNMA: ('a, 'b, 'c) t -> ('a, 'b, 'c) NMA.t
  end

module DPA :
  sig
    type ('a, 'b) t = ('a, 'b, ('a -> int)) DMA.t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a) -> ('a -> int)
				-> ('a, 'b) t
    val omega : ('a, 'b) t -> ('a -> int)
  end
  
module DBA :
  sig
    type ('a, 'b) t = ('a, 'b, ('a -> bool)) DMA.t
		
    val build : 'a Domain.t -> 'b Alphabet.alphabet -> 'a ->
                ('a -> 'b -> 'a) -> ('a -> bool)
				-> ('a, 'b) t
    val accept : ('a, 'b) t -> ('a -> bool)
	val asDPA2: ('a, 'b) t -> int -> int -> ('a, 'b) DPA.t
	val asDPA: ('a, 'b) t -> ('a, 'b) DPA.t
  end


module NMVPAFunctions : sig
	type 'a stack_iterator = 'a Iterators.iterator
end  

module NMVPA :
  sig
	type 'a nested = Internal of 'a | Push of 'a | Pop of 'a

    type ('a, 'b, 'c, 'd) t
		
    val build : 'a Domain.t -> ('b nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b nested -> 'a Iterators.iterator)	-> 
                ('a -> 'b nested -> ('a * 'd) Iterators.iterator) -> 
                ('a -> 'b nested -> 'd option -> 'a Iterators.iterator) -> 
                'c -> ('a, 'b, 'c, 'd) t
    val states : ('a, 'b, 'c, 'd) t -> 'a Domain.t
    val alphabet : ('a, 'b, 'c, 'd) t -> ('b nested) Alphabet.alphabet
    val stack : ('a, 'b, 'c, 'd) t -> 'd Domain.t
    val initial : ('a, 'b, 'c, 'd) t -> 'a
    val delta_internal : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> 'a Iterators.iterator)
    val delta_push : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> ('a * 'd) Iterators.iterator)
    val delta_pop : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> 'd option -> 'a Iterators.iterator)
    val accept : ('a, 'b, 'c, 'd) t -> 'c
  end

module NPVPA :
  sig
    type ('a, 'b, 'd) t = ('a, 'b, ('a -> int), 'd) NMVPA.t
		
    val build : 'a Domain.t -> ('b NMVPA.nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b NMVPA.nested -> 'a Iterators.iterator)	-> 
                ('a -> 'b NMVPA.nested -> ('a * 'd) Iterators.iterator) -> 
                ('a -> 'b NMVPA.nested -> 'd option -> 'a Iterators.iterator) -> 
                ('a -> int) -> ('a, 'b, 'd) t
    val omega : ('a, 'b, 'd) t -> ('a -> int)
  end
  
module NBVPA :
  sig
    type ('a, 'b, 'd) t = ('a, 'b, ('a -> bool), 'd) NMVPA.t
		
    val build : 'a Domain.t -> ('b NMVPA.nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b NMVPA.nested -> 'a Iterators.iterator)	-> 
                ('a -> 'b NMVPA.nested -> ('a * 'd) Iterators.iterator) -> 
                ('a -> 'b NMVPA.nested -> 'd option -> 'a Iterators.iterator) -> 
                ('a -> bool) -> ('a, 'b, 'd) t
    val accept : ('a, 'b, 'd) t -> ('a -> bool)
	val asNPVPA: ('a, 'b, 'd) t -> ('a, 'b, 'd) NPVPA.t
	val byNPVPA: ('a, 'b, 'd) NPVPA.t -> (int -> bool) -> ('a, 'b, 'd) t
	val complementAcceptance: ('a, 'b, 'd) t -> ('a, 'b, 'd) t
  end

module DMVPA :
  sig
	type 'a nested = 'a NMVPA.nested
    type ('a, 'b, 'c, 'd) t
		
    val build : 'a Domain.t -> ('b nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b nested -> 'a)	-> 
                ('a -> 'b nested -> ('a * 'd)) -> 
                ('a -> 'b nested -> 'd option -> 'a) -> 
                'c -> ('a, 'b, 'c, 'd) t
    val states : ('a, 'b, 'c, 'd) t -> 'a Domain.t
    val alphabet : ('a, 'b, 'c, 'd) t -> ('b nested) Alphabet.alphabet
    val stack : ('a, 'b, 'c, 'd) t -> 'd Domain.t
    val initial : ('a, 'b, 'c, 'd) t -> 'a
    val delta_internal : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> 'a)
    val delta_push : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> ('a * 'd))
    val delta_pop : ('a, 'b, 'c, 'd) t -> ('a -> 'b nested -> 'd option -> 'a)
    val accept : ('a, 'b, 'c, 'd) t -> 'c
    val asNMVPA: ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd) NMVPA.t
  end

module DPVPA :
  sig
    type ('a, 'b, 'd) t = ('a, 'b, ('a -> int), 'd) DMVPA.t
		
    val build : 'a Domain.t -> ('b DMVPA.nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b DMVPA.nested -> 'a) -> 
                ('a -> 'b DMVPA.nested -> ('a * 'd)) -> 
                ('a -> 'b DMVPA.nested -> 'd option -> 'a) -> 
                ('a -> int) -> ('a, 'b, 'd) t
    val omega : ('a, 'b, 'd) t -> ('a -> int)
  end
  
module DBVPA :
  sig
    type ('a, 'b, 'd) t = ('a, 'b, ('a -> bool), 'd) DMVPA.t
		
    val build : 'a Domain.t -> ('b DMVPA.nested) Alphabet.alphabet -> 'd Domain.t -> 'a ->
                ('a -> 'b DMVPA.nested -> 'a)	-> 
                ('a -> 'b DMVPA.nested -> ('a * 'd)) -> 
                ('a -> 'b DMVPA.nested -> 'd option -> 'a) -> 
                ('a -> bool) -> ('a, 'b, 'd) t
    val accept : ('a, 'b, 'd) t -> ('a -> bool)
	val asDPVPA2: ('a, 'b, 'd) t -> int -> int -> ('a, 'b, 'd) DPVPA.t
	val asDPVPA: ('a, 'b, 'd) t -> ('a, 'b, 'd) DPVPA.t
  end
