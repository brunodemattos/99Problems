let pack l =
	let rec aux curr acc = function
		| [] -> []
		| [x] -> (x :: curr) :: acc
		| a :: (b :: _ as t) -> if a = b then aux (a :: curr) acc t
								else aux [] ((a :: curr) :: acc) t in
	List.rev(aux [] [] l);;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
