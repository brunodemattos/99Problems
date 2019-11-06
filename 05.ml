let rec rev = function
	| [] -> []
	| h :: t -> (rev t) @ [h];;

rev [];;
rev [1];;
rev [1; 2; 3];;
