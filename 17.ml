let split l n =
	let rec first i = function
		| [] -> []
		| h :: t -> if i > n then [] else h :: (first (i+1) t)
	and
	rest i = function
		| [] -> []
		| h :: t -> if i <= n then rest (i+1) t else h :: (rest (i+1) t)
	in

	(first 1 l), (rest 1 l);;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
split ["a";"b";"c";"d"] 5;;
