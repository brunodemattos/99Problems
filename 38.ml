let rec pow a b = if b = 0 then 1 else a * (pow a (b-1));;

let encode l =
	let rec aux acc = function
		| [] -> []
		| [x] -> [(x, acc+1)]
		| a :: (b :: _ as t) -> if a = b then aux (acc+1) t else (a, acc+1) :: (aux 0 t)
	in

	aux 0 l;;

let factors n =
	let rec aux m i =
		if i > m then [] else
		if m mod i = 0 then i :: (aux (m/i) i) else aux m (i+1)
	in

	aux n 2;;

let factorList n = 
	encode (factors n);;

let phi_improved n = 
	let rec aux = function
		| [] -> 1
		| (p, m) :: t -> (p-1) * (pow p (m-1)) * (aux t)
	in
	aux (factorList n);;

let rec gcd a b = if b = 0 then a else gcd b (a mod b);;
let coprime a b = if gcd a b = 1 then true else false;;
let phi n =
	let rec aux m = if m >= n then 0 else
		if coprime n m then 1 + aux (m+1) else aux (m+1)
	in
	aux 1;;

let timeit f a = 
	let t0 = Unix.gettimeofday() in
	ignore(f a);
	let t1 = Unix.gettimeofday() in
	t1 -. t0;;

timeit phi 10090;;
timeit phi_improved 10090;;
