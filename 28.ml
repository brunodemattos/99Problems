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
		if List.length h1 <= List.length h2 then h1 :: (merge t1 (h2 :: t2))
		else h2 :: (merge (h1 :: t1) t2);;

let rec mergeSort = function
	| [] -> []
	| [x] -> [x]
	| l ->
		let l1, l2 = split l in
		merge (mergeSort l1) (mergeSort l2);;

mergeSort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
