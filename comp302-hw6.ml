exception NotImplemented

exception Msg of string

type passwd = string
type bank_account = {update_passwd  : passwd -> passwd -> unit ;
                     retrieve       : passwd -> int -> unit;
                     deposit        : passwd -> int -> unit;
                     print_balance  : passwd -> int }

(* Bank account errors *)
let wrong_pass = Msg "Wrong Password"
let too_many_attempts = Msg "Change your password"
let no_money = Msg "Insufficient funds"

(* Catalan Numbers  *)
let rec sum f i = match i with
  | 0 -> f 0
  | i -> f i + sum f (i - 1)

let rec catalan n = match n with
  | 0 -> 1
  | 1 -> 1
  | n -> sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1)

type cat_result =
  { num_rec : int;
    result  : int }

type stats =
  { entries : int ref;
    lkp : int ref }

let store : (int, int) Hashtbl.t = Hashtbl.create 1000

let reccat f = function
  | 0 | 1 -> 1
  | n -> sum (fun i -> f i * f (n - 1 - i)) (n - 1)


(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account p = 
  	let pwd = ref p in 
  	let account = ref 0 in 
  	let trials = ref 0 in
  	{
    update_passwd = (fun pi np -> 
        if pi <> !pwd then (trials := !trials + 1; raise wrong_pass)
        else (trials := 0; pwd := np));
	   
    deposit = (fun pi money -> 
    	if !trials >= 3 then raise too_many_attempts
        else if pi <> !pwd then (trials := !trials + 1; raise wrong_pass)
        else (trials := 0; account := !account + money));
	   
    retrieve = (fun pi money -> 
    	if !trials >= 3 then raise too_many_attempts
        else if pi <> !pwd then (trials := !trials + 1; raise wrong_pass)
        else if money > !account then raise no_money
        else (trials := 0; account := !account - money));
	   
    print_balance = (fun pi ->
    	if !trials >= 3 then raise too_many_attempts
        else if pi <> !pwd then (trials := !trials + 1; raise wrong_pass) 
        else (trials := 0; !account))
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec catalan_I n =
	let rec_count = ref 0 in 
	let rec catalan n = rec_count := !rec_count + 1; match n with
		| 0 -> 1
	  	| 1 -> 1
	  	| n -> sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1)
	in 
	{num_rec = !rec_count; 
	result = catalan n}
;;	
	

(* Q 2.2 : Memoization with a global store *)

let rec catalan_memo n = 
  let rec catalan n = match (Hashtbl.find_opt store n) with
    | Some v -> v
    | None -> match n with
      | 0 -> Hashtbl.add store n 1; 1
      | 1 -> Hashtbl.add store n 1; 1
      | n -> 
          let v = sum (fun i -> catalan i * catalan (n - 1 - i)) (n - 1) 
          in Hashtbl.add store n v; v
  in
  catalan n
;;


(* Q 2.3 : General memoization function *)

(* 
f : ('a -> 'b) -> 'a -> 'b
f (g : ('a -> 'b)) = function 'a -> 'b
stats : stats
return : 'a -> 'b
 *)

let memo f stats =
  let store = Hashtbl.create 1000 in
  let entries = stats.entries in
  let lkp = stats.lkp in
  let rec g a = match (Hashtbl.find_opt store a) with
    | Some v -> lkp := !lkp + 1; v 
    | None -> 
        let v = f g a 
        in entries := !entries + 1; Hashtbl.add store a v; v 
  in g
;;


(* Q 2.4 : Using memo to efficiently compute the Hofstadter Sequence Q *)

let hofstadter_Q = 
  let h_stats = {entries = ref 0; lkp = ref 0} in 
  let sequence_memo f = function  
    | 1 | 2 -> 1
    | n -> f (n - f(n -1)) + f(n - f(n - 2)) 
  in 
  let h_value = memo sequence_memo h_stats 
  in 
  fun n -> (h_value n, h_stats)
;;














