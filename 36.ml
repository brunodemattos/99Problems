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

factorList 315;;
