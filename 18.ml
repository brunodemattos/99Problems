let slice l a b =
	let rec aux i = function
		| [] -> []
		| h :: t -> if i >= a && i <= b then h :: (aux (i+1) t) else aux (i+1) t
	in

	aux 0 l;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
