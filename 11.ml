type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
	let create_tuple x count =
		if count = 1 then One x else Many (count, x)
	in

	let rec aux count acc = function
		| [] -> []
		| [x] -> (create_tuple x (count+1)) :: acc
		| a :: (b :: _ as t) -> if a = b then aux (count+1) acc t
			else aux 0 ((create_tuple a (count+1)) :: acc) t
	in

	List.rev (aux 0 [] l);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
