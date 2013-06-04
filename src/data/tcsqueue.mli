open Tcsset

module QueueUtils : sig

	val of_list: 'a list -> 'a Queue.t
	
	val of_array: 'a array -> 'a Queue.t
	
	val of_plainset: 'a TreeSet.t -> 'a Queue.t

end

module SingleOccQueue : sig

	type 'a t
	
	val create: unit -> 'a t
		
	val is_empty: 'a t -> bool
		
	val take: 'a t -> 'a
		
	val add: 'a -> 'a t -> unit

end