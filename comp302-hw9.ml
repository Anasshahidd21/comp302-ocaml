exception NotImplemented ;;

(* Suspended computation : we can suspend computation
   by wrapping it in a closure. *)
type 'a susp = Susp of (unit -> 'a) ;;
let force (Susp s) = s () ;;

type 'a lazy_list = {
  hd: 'a;
  tl: ('a lazy_opt) susp
}
and 'a lazy_opt =  ('a lazy_list) option ;;

(* Some infinite lazy lists *)
let rec ones : int lazy_list =
  {hd = 1; tl = Susp (fun () -> Some ones)} ;;
let rec many_ones : int lazy_list lazy_list =
  { hd = ones; tl = Susp (fun () -> Some many_ones) } ;;
let nat =
  let rec aux n = {hd = n; tl = Susp (fun () -> Some (aux (n+1)))}
  in aux 0 ;;


(* Create a finite lazy list *)
let rec natsFrom n =
  { hd = n ;
    tl = Susp (fun () -> natsFrom' (n-1)) }

and natsFrom' n = if n < 0 then None
  else Some (natsFrom n)

(* Alternative ...*)
(* val lazy_nats_from : int -> int lazy_opt *)
let rec lazy_nats_from n = match n with
  | 0 -> None
  | n -> Some {hd = n ; tl = Susp (fun () -> lazy_nats_from (n-1)) }

(* ---------------------------------------------------- *)

(* Processing finite objects lazily is also useful;
   it corresponds to demand driving compution.
*)
(* ---------------------------------------------------- *)
(* We define next a lazy list; this list is possibly
   finite; this is accomplished by a mutual recursive
   datatype.

   'a lazy_list defines a lazy list; we can observe the 
   head and its tail. For the tail we have two options:
   we have reached the end of the list indicated by the 
   constructor None or we have not reached the end 
   indicated by the constructor Some and we expose
   another lazy list of which we can observe the head and the tail.  

*)

(* ---------------------------------------------------- *)         
(* Q1 *)

(* 
   val take : int -> 'a lazy_list -> 'a list 
*)
let rec take n s = 
  if n = 0 then [] 
  else s.hd :: take' (n-1) (force s.tl)
         
and take' n s = match s with 
  | None -> []
  | Some opt -> take n opt

(* val map : ('a -> 'b) -> 'a lazy_list -> 'b lazy_list
*)
let rec map f s = 
  {hd = f s.hd;
   tl = Susp (fun() -> map' f (force s.tl))}
       
and map' f s = match s with
  | None -> None
  | Some opt -> Some (map f opt)
  
(* 
  val append : 'a lazy_list -> ('a lazy_list) option susp -> 'a lazy_list
*)
let rec append s1 s2 = match (force s2) with
  | None -> s1
  | Some s2' -> match (force s1.tl) with
    | None -> 
        {hd = s1.hd; 
         tl = Susp (fun() -> Some s2')}
    | Some s1t ->
        {hd = s1.hd;
         tl = Susp (fun() -> 
             Some (append s1t (Susp (fun() -> 
                 Some s2'))))} 
  
(* ---------------------------------------------------- *)
(* val interleave : 'a -> 'a list -> 'a list lazy_list *) 

let rec interleave x l = 
  let rec interleave' x l i = 
    let rec interleave_each x l i = match i with 
      | 0 -> x :: l
      | _ -> (List.hd l) :: (interleave_each x (List.tl l) (i-1))
    in
    
    {hd = interleave_each x l i;
     tl = Susp 
         (fun() -> 
            if i + 1 > ((List.length l)) then None
            else Some (interleave' x l (i+1)))}
    
  in interleave' x l 0
  
(* ---------------------------------------------------- *)
(* val flatten : 'a lazy_list lazy_list -> 'a lazy_list = <fun>
*) 
   
(* a' lazy_list lazy_list :
   hd : a' lazy_list
   tl : ((a' lazy_list lazy_list) option) susp
*)

let rec flatten s = match force s.tl with
  | None -> s.hd
  | Some s' -> append s.hd (Susp (fun () -> Some (flatten s')))

(* ---------------------------------------------------- *) 
(* val permute : 'a list -> 'a list lazy_list *)
let rec permute s = match s with
  | [] -> {hd = []; tl = Susp (fun() -> None)}
  | n :: [] -> interleave n []
  | n :: s' -> flatten (map (fun s' -> interleave n s') (permute s'))

(* ---------------------------------------------------- *)         
(* Q2 *) 
         
(* val hailstones : int -> int lazy list *)

let rec hailstones n = 
  {hd = n;
   tl = if (n mod 2 = 0) then Susp (fun() -> Some (hailstones (n/2)))
     else  Susp (fun() -> Some (hailstones (3*n + 1)))}

     