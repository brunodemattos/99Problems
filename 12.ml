type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode l =
	let rec many acc n x =
		if n = 0 then acc else many (x :: acc) (n-1) x
	in

	let rec aux acc = function
		| [] -> acc
		| One x :: t -> aux (x :: acc) t
		| Many (n, x) :: t -> aux ((many [] n x) @ (acc)) t
	in

	List.rev (aux [] l);;

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
