exception MessageException of string

module MessageChannel :
  sig
    type message_channel

	type message_listener_msg = message_channel -> string -> int -> unit
	
	type message_listener
	
    val add_named_channel : message_channel -> string -> message_channel
	
    val add_named_root_channel : string -> message_channel
	
    val add_anonymous_channel : message_channel -> message_channel
	
    val add_anonymous_root_channel : unit -> message_channel
	
    val find_channel : message_channel -> string -> message_channel
	
    val find_root_channel : string -> message_channel
	
    val get_parent_channel : message_channel -> message_channel
	
    val is_root_channel : message_channel -> bool
	
    val get_channels : message_channel -> message_channel list
	
    val get_root_channels : unit -> message_channel list
	
    val get_channel_ident : message_channel -> string
	
    val get_full_ident : message_channel -> string
	
	val channel_is_listening : message_channel -> bool
	
    val remove_channel : message_channel -> unit
	
	val new_listener: message_listener_msg -> message_listener
	
	val add_listener: message_listener -> message_channel -> unit
	
	val remove_listener: message_listener -> message_channel -> unit

    val send_message_depth : message_channel -> (unit -> string) -> int -> unit
	
    val send_message : message_channel -> (unit -> string) -> unit
	
    val increase_depth : message_channel -> unit
	
    val decrease_depth : message_channel -> unit
	
    val default_message_listener :
      bool option -> int -> (string -> unit) -> message_listener_msg
  end
