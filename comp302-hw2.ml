(* The code here will be added to the top of your code automatically.
   You do NOT need to copy it into your code.
*)

exception Domain
exception NotImplemented

type suit = Clubs | Spades | Hearts | Diamonds

type rank =  Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace

type card = rank * suit

type hand = Empty | Hand of card * hand

(* dom_suit : suit -> suit -> bool

   dom_suit s1 s2 = true iff suit s1 beats or is equal to suit s2
                    relative to the ordering S > H > D > C
   Invariants: none
   Effects: none
*)

let dom_suit s1 s2 = match s1, s2 with
  | Spades, _        -> true
  | Hearts, Diamonds -> true
  | Hearts, Clubs    -> true
  | Diamonds, Clubs  -> true
  | s1, s2           -> s1 = s2

type nat = int list (* increasing list of weights, each a power of two *)

(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) = 
  let match_num n = match n with 
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six ->  6 
  in (match_num r1) >= (match_num r2)
  
let dom_card (r1, s1)  (r2, s2) = 
  if s1 != s2 then dom_suit s1 s2  
  else dom_rank r1 r2

(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) : hand = match h with
  | Empty -> Hand (c, Empty)
  | Hand (c', h') -> if dom_card c c' then Hand (c, h) else Hand (c', insert c h')

let rec sort (h : hand) : hand = match h with
  | Empty -> Empty
  | Hand (c', h') -> if h' = Empty then Hand (c', h') else Hand insert c' (sort h')

(* Q3: Generating a deck of cards *)
let rec extract_rank ranks i count : rank = match ranks with
  | r::ranks -> if i = count then r else extract_rank ranks i (count + 1) 
          
let rec extract_suit suits i count : suit = match suits with
  | s::suits -> if i = count then s else extract_suit suits i (count + 1)

let generate_deck (suits : suit list) (ranks : rank list) : card list =
  let r_length = List.length ranks in 
    let s_length = List.length suits in 
      let rec form_list num_rank num_suit : card list =
        if suits = [] || ranks = [] then []
        else if num_rank = r_length - 1 && num_suit = s_length - 1 then ((extract_rank ranks num_rank 0), (extract_suit suits num_suit 0))::[]
        else if num_rank = r_length - 1 then ((extract_rank ranks num_rank 0), (extract_suit suits num_suit 0))::(form_list 0 (num_suit + 1))
        else ((extract_rank ranks num_rank 0), (extract_suit suits num_suit 0))::(form_list (num_rank + 1) num_suit) 
      in form_list 0 0 
  
(* Q4: Shuffling a deck of cards *)
let rec extract_card (deck : card list) (n : int) : card = match deck with 
  | card::deck' -> if n = 0 then card else extract_card deck' (n - 1) 

let rec split (deck : card list) (n : int) : card * card list = 
  let rec remaining deck n : card list = match deck with
    | card::deck' -> if n = 0 then deck' else card::(remaining deck' (n-1))
  in ((extract_card deck n), (remaining deck n))

let shuffle (deck : card list) : card list =
  let size = List.length deck in
  let rec select deck n = 
    if n = 0 then []
    else let (get_card, get_remaining) = split deck (Random.int n) in 
      get_card::(select get_remaining (n-1))                                                                      
  in select deck size

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)

(* Q1: Incrementing a sparse binary number *)
let inc (ws : nat ) : nat = 
  let rec inc_r ws = match ws with 
    | n::n1::ws' -> if n = n1 then inc_r ((n + n1)::ws') else n::n1::ws'
    | n::n1::[] -> if n = n1 then (n + n1)::[] else n::n1::[]
    | n::[] -> n::[]
  in inc_r (1::ws)

(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat =
  let rec dec_r ws = match ws with
    | [] -> raise Domain
    | n::ws' -> if n = 1 then ws' else dec_r ((n/2)::(n/2)::ws')
  in dec_r ws

(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  = match m, n with
  | [], n -> n
  | m, [] -> m
  | m, n0::nr -> add (inc m) (dec n)
  
(* Q4: Converting to integer - tail recursively *)
let rec toInt (n : nat) (acc : int) : int = 
    if n = [] then acc else toInt (dec n) (acc + 1) 

let sbinToInt (n : nat) : int =
  toInt n 0
 
















