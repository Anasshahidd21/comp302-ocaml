(* Q1-1 *)

let pow (n : int) (k : int) : int =
  	let rec pow' k = 
	  	match k with
	    | 0 -> 1
	    | k -> n * (pow' (k - 1))
	in
  		if k >= 0 then pow' k
		else raise Domain	


(* Q1-2 *)

let rec fib (n : int) : int =
	let rec fib' n  =
		match n with 
	    | 0 -> 1
	    | 1 -> 1
	    | n -> fib' (n - 1) + fib' (n - 2)
	in
		if n < 0 then raise Domain
		else fib' n

(* Q2 *)

let square_root (a : float) : float =
	let rec findroot (x : float) (acc : float) : float =
		let result = ((a /. x +. x) /. 2.0)
		in
		if abs_float(result -. x) < acc then result
		else findroot result acc
	in
		if a > 0.0 then findroot 1.0 epsilon_float
		else raise Domain


(* Q3 *)

let pow_tl n k =
	let rec aux n k acc =
		if k = 0 then acc else aux n (k-1) (acc*n)
	in
		aux n k 1

(* Q4 *)

let is_prime (n) : bool =
	let rec prime_func (n : int) (d : int) : bool = 
		d = 1 || ((n mod d != 0) && prime_func n (d-1))
	in
		if n <= 1 then raise Domain
		else prime_func n (n-1)


(* Q5 *)

let rec gcd a b =
	if a < 0 || b < 0 then raise Domain
	else if b = 0 then a else gcd b (a mod b)








