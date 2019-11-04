let drop l k =
	let rec aux i n = function
		| [] -> []
		| h :: t -> if i mod n = 0
			then aux (i+1) n t 
			else h :: (aux (i+1) n t)
	in
	aux 1 k l;;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
