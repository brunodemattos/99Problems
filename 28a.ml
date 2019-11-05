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

let rec lengthSort = function
	| [] -> []
	| [x] -> [x]
	| l ->
		let l1, l2 = split l in
		merge (lengthSort l1) (lengthSort l2);;

lengthSort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;

let rec choose k = function
	| h :: t -> if List.length h = k then h else choose k t;;

let rec remove x = function
	| [] -> []
	| h :: t -> if h = x then t else h :: (remove x t);;

let rec lengths = function
	| [] -> []
	| h :: t -> (List.length h) :: (lengths t);;


