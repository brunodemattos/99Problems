let rec gcd a b = if b = 0 then a else gcd b (a mod b);;
let coprime a b = if gcd a b = 1 then true else false;;
let phi n = 
	let rec aux m = if m >= n then 0 else
		if coprime n m then 1 + aux (m+1) else aux (m+1)
	in
	aux 1;;

phi 10;;
phi 13;;
