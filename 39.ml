let range a b = 
	let rec aux a b =
		if a > b then [] else a :: (aux (a+1) b)
	in

	if a > b then List.rev (aux b a) else aux a b;;

let is_prime n = if n <= 1 then false else
	let rec aux m = if m*m > n then true else
		if n mod m = 0 then false else aux (m+1)
	in
	aux 2;;

let all_primes a b = 
	let rec aux = function
		| [] -> []
		| h :: t -> if is_prime h then h :: (aux t) else aux t
	in
	aux (range a b);;

List.length (all_primes 2 7920);;
