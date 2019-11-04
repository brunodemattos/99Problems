let rec remove_at k = function
	| [] -> []
	| h :: t -> if k = 0 then t else (h :: (remove_at (k-1) t));;

remove_at 1 ["a";"b";"c";"d"];;
