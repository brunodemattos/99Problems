let rec extract n list = 
	if n <= 0 then [[]]
	else match list with
		| [] -> []
		| h :: t ->
			let
				contains = List.map (fun l -> h :: l) (extract (n-1) t)
			in
			let
				doesnt = extract n t
			in
			contains @ doesnt;;

extract 2 [1; 2; 3; 4];;
