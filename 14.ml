let rec duplicate = function
	| [] -> []
	| h :: t -> [h; h] @ (duplicate t);;

duplicate [1; 2; 2; 3];;
