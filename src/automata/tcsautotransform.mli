open Tcsautomata;;
open Tcsset;;

module DBAtoNBA : sig
	val transform: ('a, 'b) DBA.t -> ('a, 'b) NBA.t
end

module DBAtoDPA : sig
	val transform: ('a, 'b) DBA.t -> ('a, 'b) DPA.t
	val transform2: ('a, 'b) DBA.t -> int -> int -> ('a, 'b) DPA.t
end

module DBAtoNPA : sig
	val transform: ('a, 'b) DBA.t -> ('a, 'b) NPA.t
end

module NBAtoNPA : sig
	val transform: ('a, 'b) NBA.t -> ('a, 'b) NPA.t
end

module DPAtoNPA : sig
	val transform: ('a, 'b) DPA.t -> ('a, 'b) NPA.t
end

module NPAtoNBA : sig
	type 'a state = ('a * int) option
	val transform: ('a, 'b) NPA.t -> NPAFunctions.even_priority_set -> ('a state, 'b) NBA.t
	val size_upper_bound: NPAFunctions.even_priority_size -> NMAFunctions.state_size -> NMAFunctions.state_size
end

module DPAtoNBA : sig
	type 'a state = 'a NPAtoNBA.state
	val transform: ('a, 'b) DPA.t -> NPAFunctions.even_priority_set -> ('a state, 'b) NBA.t
	val size_upper_bound: NPAFunctions.even_priority_size -> NMAFunctions.state_size -> NMAFunctions.state_size
end

(* Piterman *)
module NBAtoDPA : sig
	type 'a piterman = Node of int * ('a TreeSet.t) * (('a piterman) TreeSet.t) 
	type 'a state = 'a piterman * int
	val transform: ('a, 'b) NBA.t -> NMAFunctions.state_size -> ('a state, 'b) DPA.t
end

module NPAtoDPA : sig
	type 'a state = ('a NPAtoNBA.state) NBAtoDPA.state
	val transform: ('a, 'b) NPA.t -> NPAFunctions.even_priority_set -> NMAFunctions.state_size -> ('a state, 'b) DPA.t
end


(* Miyano-Hayashi *)
module NcoBAtoComplementDBA : sig
	type 'a state = 'a TreeSet.t * 'a TreeSet.t
 	val transform: ('a, 'b) NBA.t -> ('a state, 'b) DBA.t
end

module GoodForGamesNBAtoNPA : sig
	type 'a state = ('a TreeSet.t * 'a TreeSet.t) list * int
	val transform: ('a, 'b) NBA.t -> NMAFunctions.state_size -> ('a state, 'b) NPA.t
end
