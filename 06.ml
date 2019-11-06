let rec rev = function
	| [] -> []
	| h :: t -> (rev t) @ [h];;

let is_palindrome l =
	l = (rev l);;

is_palindrome [];;
is_palindrome [1];;
is_palindrome [1; 2];;
is_palindrome [1; 2; 1];;
