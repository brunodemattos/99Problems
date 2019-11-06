let replicate l k = 
	let rec many x n = if n = 0 then [] else x :: (many x (n-1)) in

	let rec aux = function
		| [] -> []
		| h :: t -> (many h k) @ aux t
	in

	aux l;;

replicate ["a";"b";"c"] 3;;
