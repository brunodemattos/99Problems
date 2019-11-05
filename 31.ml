let is_prime n = if n <= 1 then false else
	let rec aux m = if m*m > n then true else
		if n mod m = 0 then false else aux (m+1)
	in
	aux 2;;

is_prime 2;;
is_prime 17;;
is_prime 77;;
is_prime 1201;;
