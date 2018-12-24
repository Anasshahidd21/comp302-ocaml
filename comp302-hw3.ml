(* -------------------------------------------------------------*)
(* QUESTION 1 : String manipulation  [20 points]                *)
(* -------------------------------------------------------------*)

(* string_explode : string -> char list *)
let string_explode s =
  tabulate (String.get s) (String.length s)
(* string_implode : char list -> string *)
let string_implode l = 
  String.concat "" (List.map (Char.escaped) l)


(* -------------------------------------------------------------*)
(* QUESTION 2 : Insert a string into a dictionary  [20 points]  *)
(* -------------------------------------------------------------*)

(* Insert a word into a dictionary. Duplicate inserts are allowed *)

(* let insert s t =
  (* ins: char list * char trie list -> char trie list *)
  let rec ins l t = match t with 
    | Node (a, kids)::t' -> 
        if List.length l = 0 then Empty::t 
        else if a = List.hd l then Node (a, (ins (List.tl l) kids)) :: t'
        else if t' != [] then Node (a, kids) :: ins l t' 
        else Node (a, kids) :: unroll l
    | Empty::[] -> Empty :: unroll l 
    | Empty::t' -> 
        if List.length l = 0 then Empty::t
        else Empty :: ins l t' 
    | [] -> unroll l
  in
  ins (string_explode s) t  *)

let insert s t = 
  (* ins: char list * char trie list -> char trie list *)
  let rec ins l t = match l with
    | [] -> Empty :: t
    | c::ct -> match t with 
      | [] -> unroll l
      | Empty::tt -> Empty :: ins l tt
      | Node (a, kids) :: tt -> 
        if a = c then Node (a, ins ct kids) :: tt
        else Node (a, kids) :: ins l tt
  in
  ins (string_explode s) t 


(* -------------------------------------------------------------*)
(* QUESTION 3 : Look up a string in a dictionary   [20 points]  *)
(* -------------------------------------------------------------*)

(* Look up a word in a dictionary *)

(* let lookup s t =
  (* lkp : char list -> char trie list -> bool *)
  let rec lkp l t = match t with 
    | Node (a, kids)::t' ->
        if List.length l = 0 then if contains_empty t then true else false
        else if a = List.hd l then lkp (List.tl l) kids
        else if t' != [] then lkp l t'
        else false 
    | Empty::t' -> 
        if List.length l = 0 then true
        else if t' != [] then lkp l t'
        else false
    | _ -> if List.length l = 0 && contains_empty t then true else false 
  in
  lkp (string_explode s) t *)

let lookup s t = 
  (* lkp : char list -> char trie list -> bool *)
  let rec lkp l t = match l with
    | [] -> contains_empty t
    | c::ct -> match t with 
      | [] -> false
      | Empty::tt -> lkp l tt
      | Node (a, kids)::tt -> 
          if a = c then lkp ct kids
          else lkp l tt
  in
  lkp (string_explode s) t

(* -------------------------------------------------------------*)
(* QUESTION 4 : Find all strings in a dictionary   [OPTIONAL]   *)
(* -------------------------------------------------------------*)

(* Find all strings which share the prefix p *)

let find_all prefix t =
  (* find_all' : char list -> char trie list -> char list list *)
  let rec find_all' l t =
    
  in
  raise NotImplemented


(* -------------------------------------------------------------*)
(* QUESTION 5 :  Logic functions   [OPTIONAL]                   *)
(* -------------------------------------------------------------*)

(* eval: labeled_pred -> labeled_pred -> int -> int -> bool *)
let eval (_, (p : int -> bool)) (_, (q : int -> bool)) (n : int) =
  raise NotImplemented
