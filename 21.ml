let rec insert_at elem k = function
	| [] -> [elem]
	| h :: t -> if k = 0 then elem :: (h :: t)
		else h :: (insert_at elem (k-1) t);;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;
