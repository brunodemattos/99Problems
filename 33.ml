let rec gcd a b = if b = 0 then a else gcd b (a mod b);;
let coprime a b = if gcd a b = 1 then true else false;;

coprime 13 27;;
coprime 20536 7826;;
