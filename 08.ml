let rec compress = function
	| a :: (b :: t) -> if a = b then compress (b :: t) else a :: (compress (b :: t))
	| smaller -> smaller;;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"; "f"; "g"; "h"];;
