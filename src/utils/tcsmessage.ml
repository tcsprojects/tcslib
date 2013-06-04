open Tcsstrings;;

exception MessageException of string

module MessageChannel = struct

	type message_channel' = {
		ident: string;
		mutable listeners: message_listener list;
		mutable sub_channels: message_channel list;
		parent: message_channel option;
		mutable depth: int;
		mutable listening: bool;
	}
	and message_channel = message_channel' ref
	and	message_listener_msg = message_channel -> string -> int -> unit
	and message_listener' = {
		message: message_listener_msg;
	}
	and message_listener = message_listener' ref
	

	let main = ref {
		listening = false;
		ident = "";
		listeners = [];
		sub_channels = [];
		parent = None;
		depth = 0
	}

	let add_named_channel channel ident =
		if ident = "" then raise (MessageException "Tcsmessage.add_named_channel: Cannot add empty title.");
		try
			let _ = List.find (fun c -> !c.ident = ident) !channel.sub_channels in
			raise (MessageException "Tcsmessage.add_named_channel: Channel already exists.");
		with Not_found -> 
			let c = ref {
				listening = !channel.listening;
				ident = ident;
				listeners = [];
				sub_channels = [];
				parent = Some channel;
				depth = 0;
			} in
			!channel.sub_channels <- c::!channel.sub_channels;
			c

	let add_named_root_channel ident =
		add_named_channel main ident
		
	let add_anonymous_channel channel =
		let c = ref {
				listening = !channel.listening;
			ident = "";
			listeners = [];
			sub_channels = [];
			parent = Some channel;
			depth = 0;
		} in
		!channel.sub_channels <- c::!channel.sub_channels;
		c
		
	let add_anonymous_root_channel _ =
		add_anonymous_channel main

	let find_channel channel ident =
		if ident = "" then raise (MessageException "Tcsmessage.find_channel: Cannot find anonymous channels.");
		try
			List.find (fun c -> !c.ident = ident) !channel.sub_channels
		with Not_found -> raise (MessageException "Tcsmessage.find_channel: Channel could not be found.")

	let find_root_channel ident =
		find_channel main ident

	let get_parent_channel channel =
		match !channel.parent with
			None -> raise (MessageException "Tcsmessage.get_parent_channel: Channel is a root channel.")
		|	Some c -> if c == main
		              then raise (MessageException "Tcsmessage.get_parent_channel: Channel is a root channel.")
					  else c

	let is_root_channel channel =
		match !channel.parent with
			None -> true
		|	Some c -> c == main

	let get_channels channel =
		!channel.sub_channels

	let get_root_channels _ =
		get_channels main

	let get_channel_ident channel =
		!channel.ident
		
	let rec get_full_ident channel =
		if is_root_channel channel
		then get_channel_ident channel
		else get_full_ident (get_parent_channel channel) ^ "->" ^ !channel.ident
		
	let channel_is_listening channel =
		!channel.listening

	let remove_channel channel =
		match !channel.parent with None -> () |
			Some p -> !p.sub_channels <- List.filter (fun c -> c != channel) !p.sub_channels

	let new_listener f = ref {
		message = f
	}
	
	let rec update_listening c =
		let listening = (!c.listeners != []) || (match !c.parent with None -> false | Some c -> !c.listening) in
		if listening != !c.listening then (
			!c.listening <- listening;
			List.iter update_listening !c.sub_channels
		)
	
	let add_listener l c =
		!c.listeners <- l::!c.listeners;
		update_listening c
	
	let remove_listener l c =
		!c.listeners <- List.filter (fun l' -> l' != l) !c.listeners;
		update_listening c

	let send_message_depth channel str depth =
		let rec helper current depth s =
			if !current.listening then (
				let d = depth + !current.depth in
				List.iter (fun f -> !f.message channel s d) !current.listeners;
				if not (is_root_channel current)
				then helper (get_parent_channel current) d s
			)
		in
			if !channel.listening
			then helper channel depth (str ())
			
	let send_message channel str =
		send_message_depth channel str 0

	let increase_depth channel =
		!channel.depth <- !channel.depth + 1

	let decrease_depth channel =
		!channel.depth <- !channel.depth - 1

	let default_message_listener identshow increment printer =
		let inline = ref false in
		let f channel str depth =
			let g str =
				if !inline
				then printer str
				else printer
					((String.make (depth * increment) ' ') ^
					(match identshow with
						None -> ""
					 |	Some false -> "[" ^ get_channel_ident channel ^ "] "
					 |	Some true -> "[" ^ get_full_ident channel ^ "] ") ^
					str);
				inline := not (String.contains str '\n')
			in
			let rec h = function [] -> ()
			|	[s] -> if s <> "" then g s
			|	x::xs -> (g (x ^ "\n"); h xs)
			in
				h (StringUtils.explode str '\n')
		in
			f
			
end;;