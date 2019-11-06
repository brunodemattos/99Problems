let rec last_two = function
	| [] -> None
	| [x] -> None
	| [a; b] -> Some (a, b)
	| x :: t -> last_two t;;

last_two [];;
last_two [1];;
last_two [1; 2];;
last_two [1; 2; 3];;
