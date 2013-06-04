open Tcsautomataparserinternal;;
open Tcsautomata

exception AutomataParserException of string * int * int

exception AutomataParserCustomException of string

val parse_automaton: (string -> unit) -> (int -> unit) -> (int -> string -> alphabet_item_type -> unit) ->
					 (int -> unit) -> (int -> string -> unit) ->
                     (int -> int -> string option -> unit) -> (int -> unit) ->
					 (int -> int -> int list -> unit) ->
					 (int -> int -> (int * int) list -> unit) ->
					 (int -> int -> int option -> int list -> unit) ->
					 (int -> int list -> unit) ->
					 (unit -> 'a) -> in_channel -> 'a
					 
type automata_type =
	NbaType of (int, int) NBA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	DbaType of (int, int) DBA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	NpaType of (int, int) NPA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	DpaType of (int, int) DPA.t * int NMAFunctions.state_iterator * int NMAFunctions.alphabet_iterator
|	NbvpaType of (int, int, int) NBVPA.t * int NMAFunctions.state_iterator * (int NMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	DbvpaType of (int, int, int) DBVPA.t * int NMAFunctions.state_iterator * (int DMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	NpvpaType of (int, int, int) NPVPA.t * int NMAFunctions.state_iterator * (int NMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator
|	DpvpaType of (int, int, int) DPVPA.t * int NMAFunctions.state_iterator * (int DMVPA.nested) NMAFunctions.alphabet_iterator * int NMVPAFunctions.stack_iterator


val parse_automaton2: in_channel -> automata_type
