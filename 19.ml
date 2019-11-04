let rotate l n = 
	let rec aux left right i = function
		| [] -> (List.rev right) @ (List.rev left)
		| h :: t -> if i <= n then aux (h::left) right (i+1) t
			else aux left (h::right) (i+1) t
	in
	aux [] [] 1 l;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
