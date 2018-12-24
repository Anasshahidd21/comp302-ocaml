exception NotImplemented

(* -------------------------------------------------------------*)
(* The MiniCAML Language                                        *)
(* -------------------------------------------------------------*)

(* Types *)
type tp =
  | Arrow of tp * tp
  | Product of tp list
  | Int
  | Bool
  | TVar of (tp option) ref

(* Expressions *)
type name = string

type primop = Equals | LessThan | Plus | Minus | Times | Negate

type exp =
  | I of int                          (* 0 | 1 | 2 | ... *)
  | B of bool                         (* true | false *)
  | If of exp * exp * exp             (* if e then e1 else e2 *)
  | Primop of primop * exp list       (* e1 <op> e2  or  <op> e *)
  | Tuple of exp list                 (* (e_1, ..., e_n) *)
  | Fn of (name * exp)                (* fn x => e *)
  | Rec of (name *  exp)              (* rec f => e *)
  | Let of (dec list * exp)           (* let decs in e end *)
  | Apply of exp * exp                (* e1 e2 *)
  | Var of name                       (* x *)

and dec =
  | Val of exp * name                  (* val x = e *)
  | Valtuple of exp * (name list)      (* val (x_1,...,x_n) = e *)

(* -------------------------------------------------------------*)
(* Unification                                                  *)
(* -------------------------------------------------------------*)

(* Different errors that can arise during unification. *)
type unif_error =
  (* Raised when attempting to unify a type variable 'a with a type t
     of which 'a is a subexpression, e.g. t is Product ['a; 'a] *)
  | UnifOccursCheckFails
  (* Raised when the unifier attempts to unify mismatched types,
     e.g. Bool with Int, or an Arrow with a Product. *)
  | UnifMismatch of tp * tp
  (* Raised when trying to unify product types with mismatched lengths *)
  | UnifProductMismatch

(* An exception constructor so that we can raise unif_error values. *)
exception UnifError of unif_error

(* Convenience function for raising unif_error values. *)
let unif_error e = raise (UnifError e)

(* `occurs a t` checks whether the type variable `a` appears in the
    type `t`.
 *)
let rec occurs a t = match t with
  | Int | Bool -> false
  | Arrow (t1, t2) -> occurs a t1 || occurs a t2
  | Product tl -> List.exists (fun t -> occurs a t) tl
  | TVar b ->
     match !b with
     | Some t' -> occurs a t'
     | None -> a == b

(* -------------------------------------------------------------*)
(* Type Inference                                               *)
(* -------------------------------------------------------------*)

type context = (name * tp) list
let empty = []

(* Represents the different kinds of type errors that can happen. *)
type type_error =
  (* When unbound variables are encountered in a program. *)
  | FreeVariable
    of name (* the name of the free variable *)

  (* When a primitive operation has the wrong number of arguments. *)
  | InvalidPrimop

(* An exception constructor, so we can throw type_error values. *)
exception TypeError of type_error

(* Convenience function for raising a type_error. *)
let type_error (e : type_error) : 'a = raise (TypeError e)

(* looks up the topmost x in ctx and returns its corresponding type *)
let rec lookup (x : name) (ctx : context) : tp option = match ctx with
  | [] -> None
  | (y, r)::rest -> if x = y then Some r else lookup x rest

(* Adds a type ascription to the context. *)
let extend ctx (x, tau) = ((x,tau)::ctx)

(* Adds multiple new type ascriptions to a context. *)
let rec extend_list ctx l = match l with
  | [] -> ctx
  | (x,tau) :: pairs -> extend_list (extend ctx (x, tau)) pairs

(* primopType p = domain, range *)
let primopType (p : primop) : tp list * tp = match p with
  | Equals   -> ([Int; Int], Bool)
  | LessThan -> ([Int; Int], Bool)
  | Plus     -> ([Int; Int], Int)
  | Minus    -> ([Int; Int], Int)
  | Times    -> ([Int; Int], Int)
  | Negate   -> ([Int], Int)

(*** Miscellaneous functions ***)

let tabulate f n =
  let rec go = function
    | i when i >= n -> []
    | i -> f i :: go (i + 1)
  in
  go 0


(* Question 1: unify *)
let rec unify (t1 : tp) (t2 : tp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | Int, Int -> ()
  | Bool, Bool -> ()
  (* For type constructors, recursively unify the parts *)
  | Arrow (t1, t1'), Arrow (t2, t2') -> 
      unify t1 t2; unify t1' t2'
  | Product tl1, Product tl2 ->
      if List.length tl1 != List.length tl2 then 
        unif_error @@ UnifProductMismatch
      else
        List.iter2 (fun a b -> unify a b) tl1 tl2
  | TVar a, _ -> unifyVar a t2
  | _, TVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t = match a with
  | {contents = Some a_tp} -> unify a_tp t
  | {contents = None} -> 
      begin match t with
        | TVar b -> (match b with
            | {contents = Some b_tp} -> unifyVar a b_tp
            | {contents = None} -> if a == b then () else a := Some (TVar b)
          )
            
        | _ ->
            if occurs a t then unif_error @@ UnifOccursCheckFails
            else a := Some t
      end


(* Question 2: infer
   Copy your code for infer from hw10, and then adjust it to work with
   the new definition of exp in the prelude.
*)
let rec infer (ctx : context) (e : exp) : tp = match e with
  
  | Var x -> (match lookup x ctx with
      | None -> type_error @@ FreeVariable x
      | Some tp -> tp
    )
    
  | I _ -> Int
    
  | B _ -> Bool
    
  | Primop (po, exps) -> 
      let (e_type_list, result_type) = primopType po in
      (match exps, e_type_list with
       | [] , [] -> result_type
       | exps, e_type_list -> 
           if List.length exps != List.length e_type_list then
             type_error @@ InvalidPrimop
           else
             List.iter2 (fun e t -> unify (infer ctx e) t) exps e_type_list; 
           result_type
      )
  
  | If (e, e1, e2) -> 
      let t0 = infer ctx e in unify t0 Bool; 
      let t1 = infer ctx e1 in let t2 = infer ctx e2 in 
      unify t1 t2; t1 
      
  | Fn (x, e) ->
      let a = TVar (ref (None)) in 
      let ctx' = extend ctx (x, a) in 
      let e_type = infer ctx' e in Arrow (a, e_type)
  
  | Apply (e0, e1) -> 
      (match infer ctx e0 with
       | Arrow (ta, tb) -> let t1 = infer ctx e1 in
           if ta = t1 then tb else unif_error @@ UnifMismatch (ta, t1)
               
       | some_shitty_type -> unif_error @@ 
           UnifMismatch (some_shitty_type, infer ctx e1)
      )
  
  | Rec (f, e) -> 
      let a = TVar (ref (None)) in 
      let ctx' = extend ctx (f, a) in 
      let e_type = infer ctx' e in Arrow (a, e_type)
  
  | Tuple es -> Product (List.map (fun e -> infer ctx e) es)
                  
  | Let ([], e) -> infer ctx e
                     
  | Let (dec :: decs, e) -> 
      let ctx' = (match dec with
          | Val (e, x) -> extend ctx (x, infer ctx e)
          | Valtuple (e, nl) -> match infer ctx e with
            | Product ets -> extend_list ctx (List.combine nl ets) 
            | nmsl -> unif_error @@ UnifMismatch (nmsl, infer ctx e)
        ) 
                               
      in
      infer ctx' (Let (decs, e))














