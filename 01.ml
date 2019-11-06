let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;


last [];;
last [1];;
last [1; 2; 3];;
