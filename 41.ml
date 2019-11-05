let is_prime n = if n <= 1 then false else
	let rec aux m = if m*m > n then true else
		if n mod m = 0 then false else aux (m+1)
	in
	aux 2;;

let goldbach n = 
	if n <= 2 then (2, 2) else
	let rec aux m =
		if is_prime m && is_prime (n-m) then (m, n-m) else aux (m+1)
	in
	aux 2;;

let rec even_range a b =
	if a > b then []
	else a :: (even_range (a+2) b);;

let smallest_even_int a =
	if a mod 2 = 0 then a else a + 1;;

let goldbach_list a b =
	let rec aux = function
		| [] -> []
		| h :: t -> (h, (goldbach h)) :: (aux t)
	in
	aux (even_range(smallest_even_int a) b);;

goldbach_list 9 20;;

let goldbach_limit a b lim = 
	let rec aux = function
		| [] -> []
		| (z, (x, y)) :: t -> 
			if (x > lim) && (y > lim) then (z, (x, y)) :: (aux t)
			else aux t
	in
	aux (goldbach_list a b);;

goldbach_limit 1 2000 50;;
