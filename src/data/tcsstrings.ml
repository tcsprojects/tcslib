module StringUtils = struct

	let rest_string s i =
		String.sub s i (String.length s - i)

	let rec explode s c =
		try
			let i = String.index s c in
			let hd = String.sub s 0 i in
			let tl = rest_string s (i + 1) in
			hd::(explode tl c)
		with Not_found -> [s]
	
	let rec implode l c = match l with
		[] -> ""
	|	[s] -> s
	|	(x::xs) -> x ^ (String.make 1 c) ^ implode xs c
		
	let indent s number c =
		let a = String.make number c in
		let l = explode s '\n' in
		let l' = List.map (fun s -> if String.length s > 0 then a ^ s else s) l in
		implode l' '\n'
		
	let index2 s c =
		try
			String.index s c
		with Not_found -> -1
		
	let rec break s len c =
		let l = String.length s in
		if l <= len then s
		else let i = index2 s '\n' in
		     if (-1 < i) && (i <= len)
			 then String.sub s 0 (i + 1) ^ break (rest_string s (i + 1)) len c
			 else (
				let b =	try
					String.rindex_from s (len - 1) ' '
				with Not_found -> len - 1
				in
				String.sub s 0 (b + 1) ^ (String.make 1 '\n') ^ break (rest_string s (b + 1)) len c
			)

	let break_and_indent s start len =
		indent (break s len ' ') start ' '
		
	let last_line_length s =
		let len = String.length s in
		try
			len - (String.rindex_from s (len - 1) '\n') - 1
		with Not_found -> len
	
	let fillup s len c =
		let len' = String.length s in
		if len' < len
		then s ^ String.make (len - len') c
		else s
		
	let fillup_left s len c =
		let len' = String.length s in
		if len' < len
		then String.make (len - len') c ^ s
		else s
		
	let to_char_array s = Array.init (String.length s) (String.get s)

end;;