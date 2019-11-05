let is_prime n = if n <= 1 then false else
	let rec aux m = if m*m > n then true else
		if n mod m = 0 then false else aux (m+1)
	in
	aux 2;;

let goldbach n = 
	let rec aux m =
		if is_prime m && is_prime (n-m) then (m, n-m) else aux (m+1)
	in
	aux 2;;

goldbach 28;;
