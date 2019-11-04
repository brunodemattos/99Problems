let factors n =
	let rec aux m i =
		if i > m then [] else
		if m mod i = 0 then i :: (aux (m/i) i) else aux m (i+1)
	in

	aux n 2;;

factors 315;;
