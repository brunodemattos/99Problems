let encode list =
    let rec aux count acc = function
    	| [] -> [] (* Can only be reached if original list is empty *)
    	| [x] -> (count+1, x) :: acc
    	| a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);;

let split l = 
	let n = List.length l in
	let rec aux l1 l2 i = function
		| [] -> (List.rev l1, List.rev l2)
		| h :: t -> 
			if i <= n/2 then aux (h :: l1) l2 (i+1) t
			else aux l1 (h :: l2) (i+1) t
	in
	aux [] [] 1 l;;

let rec merge l1 l2 = 
	match l1, l2 with
	| [], [] -> []
	| l, [] -> l
	| [], l -> l
	| h1 :: t1, h2 :: t2 ->
		if h1 <= h2 then h1 :: (merge t1 (h2 :: t2))
		else h2 :: (merge (h1 :: t1) t2);;

let rec mergeSort = function
	| [] -> []
	| [x] -> [x]
	| l ->
		let l1, l2 = split l in
		merge (mergeSort l1) (mergeSort l2);;

let rec choose k = function
	| [] -> raise Not_found
	| h :: t -> if (List.length h) = k then h else choose k t;;

let rec remove x = function
	| [] -> []
	| h :: t -> if h = x then t else h :: (remove x t);;

let rec lengths = function
	| [] -> []
	| h :: t -> (List.length h) :: (lengths t);;

let frequency_sort l =
	let sizes = lengths l in
	let sorted = mergeSort sizes in
	let rec aux lAux = function
		| [] -> []
		(*Need to find someone of size h*)
		| h :: t -> 
			let chosen = choose h lAux in
			let newList = remove chosen lAux in
			chosen :: (aux newList t)
	in
	aux l sorted;;

frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
