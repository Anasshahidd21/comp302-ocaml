exception NotImplemented

(* Spreadsheet

   We model a column in a spreadsheet as a list and a spreadsheet as a
   list of columns.
*)

(* In the spreadsheet we store arithmetic expressions. The simplest
   arithmetic expression is a number; more complex arithmetic
   expressions can be formed using the constructors Plus and Times.

   Each node in the abstract syntax tree for expressions stores its
   value. The value of a number is simply an integer. The value of a
   Plus-expression is the sum of the values of the two subtrees; the
   value of a Times-expression is the product of the values of the
   values of the two subtrees.
*)

type exp =
| Num of int ref
| Plus of int ref * exp * exp
| Times of int ref * exp * exp

type column = exp list
type spreadsheet = column list

(* Example spreadsheet *)
let m = [ [ Num (ref 0) ; Num (ref 1); Num (ref 2)] ;
          [ Num (ref 4) ; Num (ref 7) ; Num (ref 9)] ;
          [ Num (ref 2); Num (ref 3); Num (ref 10)] ]

(* Helper function: get the value at a node in the abstract syntax
   tree of an arithmetic expression *)
let get_value e = match e with
  | Num r -> !r
  | Plus (r, _, _ ) -> !r
  | Times (r, _, _ ) -> !r

(* Create a number expression *)
let num x = Num x

(* Initialize a spreadsheet from an int ref list list *)
let initialize m = List.map (fun c -> List.map (fun x -> Num x) c) m

(* Add a column at the end of the spreadsheet *)
let add_column m c = m@[c]

(** Transforms a function that collapses integer lists into one for
 * exps. *)
let lift_to_exp (f : int list -> int) (xs : exp list) : exp =
  Num (ref (f (List.map get_value xs)))

(* Test functions *)
let sum =
  ( "sum"
   , lift_to_exp (fun xs -> List.fold_left (+) 0 xs)
  )

let product =
  ( "product"
  , lift_to_exp (fun xs -> List.fold_left ( * ) 1 xs)
  )

let average =
  ( "average"
  , lift_to_exp
      (function
       | [] -> 0 (* return a zero average for empty columns *)
       | xs ->
          let rec a sum len =
            function
            | [] -> (sum, len)
            | (x::xs) -> a (x + sum) (1 + len) xs
          in
          let (sum, len) = a 0 0 xs in
          sum / len)
  )


(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 1 *)
(* Helper function: given two expressions, we add or multiply
   them     *)
(* ————————————————————–—————————————————————————————————————————————– *)

let add e1 e2 = 
  let sum = get_value e1 + get_value e2 in 
  Plus (ref sum, e1, e2)
  
let mult e1 e2 =
  let prod = get_value e1 * get_value e2 in 
  Times (ref prod, e1, e2)

(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 2                                                        *)
(* compute_column m f = c

   Given a spreadsheet m and a function f, compute the i-th value in
   the result column c by using the i-th value from each column in m.

   Example:
   m = [ [a1 ; a2 ; a3 ; a4] ;
         [b1 ; b2 ; b3 ; b4] ;
         [c1 ; c2 ; c3 ; c4] ]

  To compute the 2nd value in the new column, we call f with
  [a2 ; b2 ; c2]

   Generic type of compute_column:
     'a list list -> ('a list -> 'b) -> 'b list

   If it helps, you can think of the specific case where we have a
   spreadsheet containing expressions, i.e.
   compute_column: exp list list -> (exp list -> exp) -> exp list

   Use List.map to your advantage!

   Carefully design the condition when you stop.
*)
(* ————————————————————–—————————————————————————————————————————————– *)

let rec compute_column sheet (_, f) = 
  let rec transpose n = 
    if n < List.length (List.hd sheet) then
      List.map (fun x -> List.nth x n) sheet :: transpose (n+1) 
    else []
  in
  List.map f (transpose 0) 

(* ————————————————————–—————————————————————————————————————————————– *)
(* QUESTION 3 *)
(* Implement a function update which given an expression will re-
   compute the values stored at each node. This function will be used
   after we have updated a given number.

   update  : exp -> unit

*)
(* ————————————————————–—————————————————————————————————————————————– *)

let update expr = 
  let rec eval expr = match expr with
    | Num i -> ()
    | Plus (i, exp0, exp1) -> eval exp0; eval exp1; 
        i := get_value exp0 + get_value exp1
    | Times (i, exp0, exp1) -> eval exp0; eval exp1;
        i := get_value exp0 * get_value exp1 
  in eval expr 

let update_sheet sheet = 
  List.map update (List.fold_left (@) [] sheet); ()
                                        

(* EXTRA FUN:
   Our implementation traverses the whole expression and even worse
   the whole spreadsheet, if one number cell is being updated.

   If you are looking for a nice programming problem, think
   about how to update only those values which are parent nodes to the
   Number being updated. You might need to choose a different
   representation for expressions.

*)
(* ————————————————————–—————————————————————————————————————————————– *)
