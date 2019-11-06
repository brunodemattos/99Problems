let encode l =
	let rec aux curr acc = function
		| [] -> []
		| [x] -> (curr + 1, x) :: acc
		| a :: (b :: _ as t) ->
			if a = b
				then aux (curr+1) acc t
				else aux 0 ((curr+1, a) :: acc) t in
		List.rev (aux 0 [] l);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
