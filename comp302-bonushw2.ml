exception NotImplemented

(** The unfold function. *)
let rec unfold (f : 'seed -> 'a * 'seed) (stop : 'seed -> bool) (b : 'seed) : 'a list =
  if stop b then
    []
  else
    let x, b' = f b in
    x :: unfold f stop b'

(** Example function using `unfold`.
 * Generates a list of natural numbers less than `max`.
 * *)
let nats max = unfold (fun b -> (b, b+1)) (fun b -> max <= b) 0


(* Question 1: let's compose something! *)

(* 1.1 Composition *)

let compose (fs : ('a -> 'a) list) : 'a -> 'a =
  List.fold_right (fun x -> x) fs 
    
(* 1.2 Replication *)

let replicate (n : int) : 'a -> 'a list = 
  let rec replicate' n x = match n with
    | 0 -> []
    | n -> x :: replicate' (n - 1) x
  in replicate' n
  
(* 1.3 Repeating *)

let repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
  compose (replicate n f) 
  
  
  
(* Question 2: unfolding is like folding in reverse *)

(* 2.1 Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list = 
  unfold (fun n -> (n, n+2)) (fun n -> max <= n) 0
  
(* 2.2 Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list =
  unfold (fun (n, m) -> (n, (m, n+m))) (fun (n, m) -> max <= n) (1, 1)

(* 2.3 Compute Pascal's triangle up to a maximum row length. *)
let pascal (max : int) : int list list = 
  unfold (fun l -> (l, 1 :: (List.map2 (fun x y -> x + y) l (List.tl l @ [0])))) 
    (fun l -> max < List.length l) 
    [1] 
  
(* 2.4 Implement the zip, which joins two lists into a list of tuples.
 * e.g. zip [1;2] ['a', 'c'] = [(1, 'a'); (2, 'c')]
 * Note that if one list is shorter than the other, then the resulting
 * list should have the length of the smaller list. *)
let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  unfold (fun (x::xs, y::ys) -> ((x,y), (xs, ys))) 
    (fun (x, y) -> x = [] or y = []) 
    (l1, l2)