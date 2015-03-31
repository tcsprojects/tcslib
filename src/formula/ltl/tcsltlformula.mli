type ltl_formula = 
	FProp of string
  | FTT
  | FFF
  | FNeg of ltl_formula
  | FAnd of ltl_formula * ltl_formula
  | FOr of ltl_formula * ltl_formula
  | FNext of ltl_formula
  | FRelease of ltl_formula *ltl_formula
  | FUntil of ltl_formula * ltl_formula

val eval_metaformula : Tcsmetaformula.formula_expr -> ltl_formula

val formula_length: ltl_formula -> int

val format_formula : ltl_formula -> string

val formula_to_positive : ltl_formula -> ltl_formula

val is_positive : ltl_formula -> bool


type decomposed_ltl_formula_part =
    FIntAtom of bool (* player *)
  | FIntProp of bool * int (* negation * proposition reference *)
  | FIntNext of int (* formula *)
  | FIntBranch of bool * int * int (* player * left formula * right formula *)
  | FIntFixpoint of bool * int * int (* player * left formula * right formula *)

type decomposed_ltl_formula =
	int *
	decomposed_ltl_formula_part array *
	(int array) array * (* contains additional links from formula to formulas *)
	(string * (int * int)) array (* prop, positive formula, negative formula *)

val normal_form_formula_to_decomposed_formula :
  ltl_formula -> (ltl_formula -> ltl_formula array) -> decomposed_ltl_formula

val sort_decomposed_formula :
  decomposed_ltl_formula ->
  (decomposed_ltl_formula ->
   decomposed_ltl_formula_part ->
   decomposed_ltl_formula_part -> int) ->
  decomposed_ltl_formula

val get_formula_depth:
   decomposed_ltl_formula ->
   decomposed_ltl_formula_part -> int

val decomposed_formula_to_formula :
  decomposed_ltl_formula -> int -> ltl_formula

val format_decomposed_formula :
  decomposed_ltl_formula -> int -> string
	
val ltl_formula_link_map: ltl_formula -> ltl_formula array
	