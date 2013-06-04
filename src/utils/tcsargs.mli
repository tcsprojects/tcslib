module SimpleArgs : sig
	type arg_spec = Arg.spec
	type arg_key = Arg.key list
	type arg_doc = Arg.doc
	val parse :
	  (arg_key * arg_spec * arg_doc) list ->
	  Arg.anon_fun ->
	  Arg.usage_msg ->
	  (Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'a) ->
	  (string -> Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'b) ->
	  unit
	val parsearr :
	  string array ->
	  (arg_key * arg_spec * arg_doc) list ->
	  Arg.anon_fun ->
	  Arg.usage_msg ->
	  (Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'a) ->
	  (string -> Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'b) ->
	  unit
	val parsestr :
	  string ->
	  (arg_key * arg_spec * arg_doc) list ->
	  Arg.anon_fun ->
	  Arg.usage_msg ->
	  (Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'a) ->
	  (string -> Arg.usage_msg -> (arg_key * arg_spec * arg_doc) list -> 'b) ->
	  unit
	val argprint_help : string -> (arg_key * arg_spec * arg_doc) list -> unit
	val argprint_bad :
	  string -> string -> (arg_key * arg_spec * arg_doc) list -> unit
	val parsedef :
	  (arg_key * arg_spec * arg_doc) list ->
	  Arg.anon_fun -> Arg.usage_msg -> unit
end

module CustomArgs : sig
  
    type arg_type =
        Unit of (unit -> unit)
      | Bool of (bool -> unit)
      | String of (string -> unit)
      | Int of (int -> unit)
      | Float of (float -> unit)
	  | Option of (string * (unit -> unit)) list
      | Tuple of arg_type list
      | Multiple of (int -> (int -> string) -> int)
	  
    type category
	
    type argument_item
	
	type arguments_object
	
    type args_exception_type =
        ArgsUnknownArgument of string
	  | ArgsCustomFailure of string
      | ArgsWrongParameter of argument_item * int * string * string
      | ArgsRequiredParameter of argument_item * int * string
      | ArgsRequiredArgument of argument_item
      | ArgsMultipleOccurrence of argument_item
	  
    exception ArgsException of args_exception_type
	
    val new_object : string -> string -> bool -> string list -> arguments_object

    val new_category : string -> string -> category

    val argument_ident : argument_item -> string

    val register_argument :
      arguments_object ->
      argument_item option ->
      bool ->
      bool ->
      category option ->
      string list -> string -> (unit -> string) -> arg_type -> argument_item

    val format_arg_type : arg_type -> string
  
    val format_exception : args_exception_type -> string
    
	val error_function : arguments_object -> (string -> unit) -> args_exception_type -> unit
    
	val help_function : arguments_object -> (string -> unit) -> int -> (int -> string) -> unit
    
	val register_help_function :
		arguments_object -> string list -> string -> (unit -> string) -> (string -> unit) -> argument_item
    
	val parse_arguments : arguments_object -> string array -> int -> unit
    
	val parse_arguments_with_error :
      arguments_object ->
      string array -> int -> (args_exception_type -> unit) -> unit

end
