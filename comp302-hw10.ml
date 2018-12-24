exception NotImplemented

(* The type of a test.
 * A test is a list of pairs of inputs and expected outputs. *)
type ('i, 'o) tests = ('i * 'o) list


(* -------------------------------------------------------------*)
(* The MiniCAML Language                                        *)
(* -------------------------------------------------------------*)

(* Types *)
type tp =
  | Arrow of tp * tp (* function type: S -> T *)
  | Product of tp list (* tuples: T1 * T2 * ... * Tn *)
  | Int
  | Bool

(* Used for variables, aka "identifiers" *)
type name = string

(** The primitive operations available in MiniCAML. *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate

(* Expressions *)
type exp =
  | I of int                          (* 0 | 1 | 2 | ... *)
  | B of bool                         (* true | false *)
  | If of exp * exp * exp             (* if e then e1 else e2 *)
  | Primop of primop * exp list       (* e1 <op> e2  or <op> e *)
  | Tuple of exp list                 (* (e_1, ..., e_n) *)
  | Fn of (name * tp * exp)           (* fn (x:t) => e *)
  | Rec of (name * tp * exp)          (* rec (f:t) => e *)
  | Let of (dec list * exp)           (* let decs in e end *)
  | Apply of exp * exp                (* e1 e2 *)
  | Var of name                       (* x *)

(* Declarations associate names with expressions.
 * Note that the pair is "flipped" from what is written in the comment. *)
and dec =
  | Val of exp * name                  (* val x = e *)
  | Valtuple of exp * (name list)      (* val (x_1, ..., x_n) = e *)

(* -------------------------------------------------------------*)
(* Helpers for manipulating lists of variable names             *)
(* -------------------------------------------------------------*)

(* Decides whether x is an element of l. *)
let member (x : 'a) (l : 'a list) = List.exists (fun y -> y = x) l

(* Takes the union of two lists.
   If both lists are in fact sets (all element are unique) then the
   output will also be a set. *)
let rec union (p : 'a list * 'a list) = match p with
  | ([], l) -> l
  | (x :: t, l) ->
    if member x l then
      union (t, l)
    else
      x :: union (t, l)

(** Takes the union of a list of lists. *)
let unions (p : 'a list list) =
  List.fold_right (fun x y -> union (x, y)) p []

(* Deletes each element of a list of entries from another list.
    e.g. delete [y] [x; y; z] = [x; z] *)
let rec delete vl vlist = match vlist with
  | [] -> []
  | h :: t ->
    if member h vl then delete vl t
    else h :: delete vl t

(* Deletes all instances of an element from a list. *)
let delete_var v vlist = delete [v] vlist
;;

(* -------------------------------------------------------------*)
(* Calculating the free variables of an expression              *)
(* -------------------------------------------------------------*)

let boundVars d = match d with
  | Val (_, name) -> [name]
  | Valtuple (_, names) -> names

let rec freeVarsDec d = match d with
  | Val (e, _) -> freeVariables e
  | Valtuple (e, _) -> freeVariables e

(* freeVariables e = list of names occurring free in e
   Invariant: every name occurs at most once.

   The algorithm works from the leaves of the expression tree
   upwards. Every time a variable is encountered, it is considered free.
   When a binding construct is encountered (e.g. Let) the declared
   variable is deleted from the set of free variables formed by the union
   of the recursive calls.
   Other constructs simply form the union of the sets of free variables
   from the recursive calls and return it.
 *)
and freeVariables e = match e with
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) ->
    union (freeVariables e, union (freeVariables e1, freeVariables e2))
  | Primop (po, args) ->
    List.fold_right (fun e1 e2 -> union (freeVariables e1, e2)) args []
  | Tuple exps ->
    List.fold_right (fun s1 s2 -> union (freeVariables s1, s2)) exps []
  | Fn (x, _,  e) ->
    delete_var x (freeVariables e)
  | Rec (x, t, e) ->
      delete_var  x (freeVariables e)
  | Let ([] , e) -> freeVariables e
  | Let (dec::decs, e2) ->
     let fv = freeVarsDec dec  in
     let bv = boundVars dec in
     union (fv, delete bv (freeVariables (Let(decs, e2))))
  | Apply (e1, e2) ->
      union(freeVariables e1, freeVariables e2)
;;

(* -------------------------------------------------------------*)
(* Substitution                                                 *)
(* -------------------------------------------------------------*)

(** A substitution (e/x)
    This is read as "e for x".
 *)
type subst = exp * name;;

(* -------------------------------------------------------------*)
(* Evaluation                                                   *)
(* -------------------------------------------------------------*)

exception Stuck of string

(* Evaluates a primitive operation *)
let evalOp (op : primop * exp list) = match op with
  | (Equals,   [I i; I i']) -> Some (B (i = i'))
  | (LessThan, [I i; I i']) -> Some (B (i < i'))
  | (Plus,     [I i; I i']) -> Some (I(i + i'))
  | (Minus,    [I i; I i']) -> Some (I(i - i'))
  | (Times,    [I i; I i']) -> Some (I(i * i'))
  | (Negate,   [I i])       -> Some (I(-i))
  | _                       -> None
;;

(* -------------------------------------------------------------*)
(* Type Inference                                               *)
(* -------------------------------------------------------------*)

exception TypeError of string
exception NotFound

(* Type contexts *)
type context = (name * tp) list
let empty = []

(* Looks up the topmost x in ctx and returns its corresponding type.
   If the variable x cannot be located in ctx, raises NotFound.
 *)
let rec lookup x ctx = match ctx with
  | [] -> raise NotFound
  | (y, r)::rest -> if x = y then r else lookup x rest

(* Adds a new type ascription to a context. *)
let extend ctx (x, tau) = ((x,tau)::ctx)

(* Adds multiple new type ascriptions to a context. *)
let rec extend_list ctx l = match l with
  | [] -> ctx
  | (x,tau) :: pairs -> extend_list (extend ctx (x, tau)) pairs

(* Computes the type of a primitive operation.
   The result is a tuple representing the domain and range of the primop.
 *)
let primopType (p : primop) : tp list * tp = match p with
  | Equals   -> ([Int; Int], Bool)
  | LessThan -> ([Int; Int], Bool)
  | Plus     -> ([Int; Int], Int)
  | Minus    -> ([Int; Int], Int)
  | Times    -> ([Int; Int], Int)
  | Negate   -> ([Int], Int)

(* Converts a type to a string representation. *)
let rec string_of_tp t = match t with
  | Arrow (t1, t2) -> string_of_tp  t1 ^ " -> " ^ string_of_tp t2
  | Int -> "int"
  | Bool -> "bool"
  | Product tl -> String.concat " * " (List.map string_of_tp tl)

(* type_mismatch e i throws a type error of the form:
   "Expected type e
    Found type i"
 *)
let type_mismatch expected_type inferred_type =
raise (TypeError ("Expected type " ^ string_of_tp expected_type ^
                  "\nFound type " ^ string_of_tp inferred_type ))
;;


(* -------------------------------------------------------------*)
(* Other helper functions                                       *)
(* You don't need to look at these to do the assignment, but it *)
(* would be a good idea to understand them.                     *)
(* -------------------------------------------------------------*)

(* Generating fresh (new) variable names *)
type gen_var =
  { fresh : name -> name (* generates a fresh name based on a given one. *)
  ; reset : unit -> unit (* resets the internal counter for making names. *)
  }

let gen_var : gen_var =
  let counter = ref 0 in
  {fresh = (fun x -> incr counter; string_of_int (!counter) ^ x) ;
   reset = fun () ->  counter := 0}

let freshVar = gen_var.fresh
let resetCtr = gen_var.reset
;;


(* String representations of expressions. Useful for debugging! *)
let nl_sep l = String.concat "\n" l

let string_of_op p = match p with
  | Equals   -> " = "
  | LessThan -> " < "
  | Plus     -> " + "
  | Minus    -> " - "
  | Times    -> " * "
  | Negate   -> "-"

let rec string_of_exp indent =
  let new_ind = indent ^ "  " in
  function
  | I n -> string_of_int n
  | B b -> (if b then "True" else "False")
  | If (p, e1, e2) -> nl_sep
    [
    "if " ^ (string_of_exp new_ind p);
    new_ind ^ "then " ^ (string_of_exp new_ind e1);
    new_ind ^ "else " ^ (string_of_exp new_ind e2)
    ]
  | Primop (p, el) ->
    if p = Negate
      then (string_of_op p) ^ (string_of_exp indent (List.nth el 0))
      else
      (string_of_exp indent (List.nth el 0)) ^ (string_of_op p) ^ (string_of_exp indent (List.nth el 1))
  | Tuple l ->
    "(" ^ (String.concat ", " (List.map (string_of_exp new_ind) l)) ^ ")"
  | Fn (name, tp, exp) ->
    "fun (" ^ name ^ ":" ^ (string_of_tp tp) ^ ") => " ^ (string_of_exp new_ind exp)
  | Rec (name, tp, exp) ->
    "rec (" ^ name ^ ":" ^ (string_of_tp tp) ^ ") = " ^ (string_of_exp new_ind exp)
  | Let (decs, e) -> nl_sep
    [
    "let";
    string_of_decs new_ind decs;
    indent ^ "in " ^ (string_of_exp new_ind e)
    ]
  | Apply (e1, e2) -> (string_of_exp indent e1) ^ " " ^ (string_of_exp indent e2)
  | Var name -> name

and string_of_decs indent decs =
  let new_ind = indent ^ "  " in
  let single dec = match dec with
    | Val(e, name) -> indent ^ name ^ " = " ^ (string_of_exp new_ind e)
    | Valtuple(e, l) -> indent ^ "(" ^ (String.concat ", " l) ^ ") = " ^ (string_of_exp new_ind e)
  in
  nl_sep (List.map single decs)
;;


(***** Question 1a: unused variables *****)

let rec unused_vars e = match e with
  | Var _ | I _ | B _ -> []
                         
  | If (e, e1, e2) ->
      union (unused_vars e, union (unused_vars e1, unused_vars e2))
        
  | Primop (po, args) ->
      List.fold_right (fun e1 e2 -> union (unused_vars  e1, e2)) args []
        
  | Apply (e1, e2) -> union (unused_vars e1, unused_vars e2)
                        
  | Fn (x, _, e) | Rec (x, _, e) ->
      if member x (freeVariables e) then
        unused_vars e
      else
        union ([x], unused_vars e)

  | Tuple exps -> 
      List.fold_right (fun e1 e2 -> union (unused_vars e1, e2)) exps []
  
  | Let ([], e) -> unused_vars e

  | Let (Val (e, x) :: decs, e2) -> begin match decs with 
      | [] -> union (unused_vars e2, 
                     delete (freeVariables e2) (union (unused_vars e, [x])))
      | Val (e', x') :: decs' -> 
          union (union ((unused_vars e), [x]), 
                 unused_vars (Let (Val (e', x') :: decs', e2)))
      | Valtuple (e', nlist) :: decs' ->
          union (union ((unused_vars e), [x]), 
                 unused_vars (Let (Valtuple (e', nlist) :: decs', e2))) 
    end
    
  | Let (Valtuple (e, xl) :: decs, e2) -> begin match decs with
      | [] -> union (unused_vars e2, 
                     delete (freeVariables e2) (union (unused_vars e, xl)))
      | Valtuple (e', nlist) :: decs' ->
          union (union ((unused_vars e), xl), 
                 unused_vars (Let (Valtuple (e', nlist) :: decs', e2))) 
      | Val (e', x') :: decs' -> 
          union (union ((unused_vars e), xl), 
                 unused_vars (Let (Val (e', x') :: decs', e2)))
    end
    
(* Question 1b: write your own tests for unused_vars *)

let unused_vars_tests : (exp, name list) tests =
  [(Fn ("x", Int, Primop (Plus, [Var "x"; I 5])), 
    []);
   
   (Let ([Valtuple (I 6, ["a"; "c"])], Tuple [Var "a"; Var "b"]), 
    ["c"]);
  
   (Let ([Val (I 5, "x"); Val (I 4, "y")], 
         Fn ("a", Int, Primop (Times, [Var "a"; I 3]))), 
    ["x"; "y"]);
   
   (Let ([], Tuple [Var "a"]), 
    []); 
   
   (Let ([Valtuple (I 2, ["x"; "y"])], 
         Fn ("x", Int, 
             Let ([Val (I 8, "p")], 
                  Fn ("p", Int, Primop (Times, [Var "p"; I 4]))))),
    ["x"; "y"; "p"]);
  ]

(* Question 2a: substitution *)

(* Some helper function for Question 2a *)
let rec replace_in_list list o n = match list with
  | [] -> []
  | e :: list' -> 
      if e = o then n :: (replace_in_list list' o n) 
      else e :: (replace_in_list list' o n)

(** Applies the substitution s to each element of the list a. *)
let rec substArg s a = List.map (subst s) a

(** Applies the substitution (e', x), aka s, to exp.
    To implement some of the missing cases, you may need to use the
    `rename` function provided below. To see why, see the comment
    associated with `rename`.
*)
and subst ((e', x) as s) exp = match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, substArg s args)
  | If (e, e1, e2) ->
      If(subst s e, subst s e1, subst s e2)
  | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
  | Fn (y, t, e) ->
      if y = x then
        Fn (y, t, e)
      else
      if member y (freeVariables e') then
        let (y,e1) = rename (y,e) in
        Fn (y, t, subst s e1)
      else
        Fn(y, t, subst s e)
  | Rec (y, t, e) ->
      if y = x then
        Rec (y, t, e)
      else
      if member y (freeVariables e') then
        let (y, e1) = rename (y,e) in
        Rec (y, t, subst s e1)
      else
        Rec (y, t, subst s e)

  | Tuple es -> 
      Tuple (List.map (subst s) es)

  | Let ([], e2) -> Let ([], subst s e2)
  
  | Let (dec1 :: decs, e2) -> begin match dec1 with
      | Val (exp, name) -> 
          let new_e1 = subst s exp in
          if name = x
          then 
            Let ((Val (new_e1, name) :: decs), e2)
              
          else 
          if member name (freeVariables e') then
            let new_exp = rename (name, Let (decs, e2)) in
            match subst s (snd new_exp) with
            | Let (dl, in_exp) -> 
                Let ((Val (new_e1, fst new_exp) :: dl), in_exp)
          
          else begin match subst s (Let (decs, e2)) with
            | Let (dl, in_exp) -> 
                Let ((Val (new_e1, name)) :: dl, in_exp)
          end 
  
      | Valtuple (exp, names) -> 
          if member x names then
            Let ((Valtuple (subst s exp, names)) :: decs, e2)
          else 
            let new_exp = renameList names e' (Let (decs, e2)) in
            begin match subst s (snd new_exp) with
              | Let (dl, in_exp) -> 
                  Let ((Valtuple (subst s exp, fst new_exp)) :: dl, in_exp)
            end 
                                  
    end
                                  

and substList l e = match l with
  | [] -> e
  | (x,e')::pairs ->
      subst (x,e') (substList pairs e)

(** Replaces the variable x with a fresh version of itself.

    This is an important operation for implementing substitutions that
    avoid capture.
    e.g. If we naively compute [y/x](fun y => x) we get (fun y => y),
    but this doesn't have the right interpretation anymore; the
    variable y that we plugged in got "captured" by the `fun y`
    binder.
    The solution is to observe that the variable y introduced by the
    fun-expression *appears free* in the expression we are substituting for x.
    In this case, we must rename the bound variable y (introduced by
    `fun y`).
    e.g We want to compute [y/x](fun y => x). We first rename the
    bound variable y, and instead compute [y/x](fun y1 => x).
    This gives (fun y1 => y), which has the right interpretation.
*)
and rename (x, e) =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

(** Performs multiple renamings on the same expression. *)
and renameAll e = match e with
  | ([], e) -> ([], e)
  | (x::xs, e) ->
      let (x', e) = rename (x, e) in
      let (xs', e) = renameAll (xs, e) in
      (x' :: xs', e)

and renameList names e' exp =
  if List.exists (fun name -> member name (freeVariables e')) names then
    renameAll(names, exp)
  else
    (names, exp) 

(* Question 2b: write your own tests for subst *)

let subst_tests : (subst * exp, exp) tests =
  [ ( ( (Var "x", "y"), Tuple [ Var "y" ] )
    , Tuple [ Var "x" ]
    );

    ( ( (Primop(Plus, [Var "y"; Var "x"]), "x"), Fn ("y", Int, Var "x" ))
    , Fn("1y", Int, Primop(Plus, [Var "y"; Var "x"])  ));
    (* fun y -> x *)

    ( ( (Var "x", "y"), Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])))
    , Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])));

    (* let y = 3 in y + 1 *)

    ( ( (Var "x", "y"), Let([Val (I 1, "x")], Primop(Plus, [Var "y"; Var "x"])))
    , Let([Val (I 1, "1x")], Primop(Plus, [Var "x"; Var "1x"])));

    (* subst (Var "x", "y") (Let([Val (I 1, "x")], Primop(Plus, [Var "y"; Var "x"]))) *)
    (* let x = 1 in x + y *)
    (* let x1 = 1 in x1 + x *)
    
    ( ( (Var "x", "y"), Let([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"])))
    , Let ([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"])));
    
   (* subst (Var "x", "y") (Let([Val (I 1, "x"); Val (I 4, "y")], Primop(Plus, [Var "y"; Var "x"]))) *)
   (* let x = 1, y = 4 in x + y *)
   (* let x = 1, y = 4 in x + y *)
    
    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "z"])], Primop(Plus, [Var "x"; Var "y"])));
    
   (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"]))) *)
   (* let (y, z) = (3, 4) in x + y *)
   (* let (y, z) = (3, 4) in x + y *)

    ( ( (Var "x", "y"), Let ([Val (Fn ("y", Int, I 2), "x")], Primop(Plus, [Var "x"; Var "z"])))
    , Let ([Val (Fn ("x", Int, I 2), "1x")], Primop(Plus, [Var "1x"; Var "z"])));
    (* let x = (fun y -> 2) in x + z *)
    (* let x1 = (fun x -> 2) in x1 + z *)


    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "z"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"])));

    (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "z"])], Primop(Plus, [Var "x"; Var "y"]))) *)
    (* let (y, z) = (3, 4), x = 5 in x + y *)
    (* let (y, z) = (3, 4), x = 5 in x + y *)

    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "a"]); Val( I 5, "x")], Primop(Plus, [(Primop(Plus, [Var "x"; Var "y"])); Var "z"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "a"]); Val( I 5, "1x")], Primop(Plus, [(Primop(Plus, [Var "1x"; Var "y"])); Var "x"])));

    (* subst (Var "x", "z") (Let([Valtuple(Tuple([I 3; I 4]),["y"; "a"]); Val( I 5, "x")], Primop(Plus, [Var "x"; Var "y"; Var "z"]))) *)
    (* let (y, a) = (3, 4), x = 5 in x + y + z *)
    (* let (y, a) = (3, 4), x1 = 5 in x1 + y + x*) 

    ( ( (I 3, "x"), Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])))
    , Let([Val (I 3, "y")], Primop(Plus, [Var "y"; I 1])));
    
    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "x"]); Val( I 5, "a")], Primop(Plus, [(Primop(Plus, [Var "x"; Var "y"])); Var "z"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "1x"]); Val( I 5, "a")], Primop(Plus, [(Primop(Plus, [Var "1x"; Var "y"])); Var "x"])));
    
    ( ( (Var "x", "z"), Let([Valtuple(Tuple([I 3; I 4]),["y"; "x"]); Val( I 5, "x")], Primop(Plus, [(Primop(Plus, [Var "x"; Var "y"])); Var "z"])))
    , Let ([Valtuple (Tuple ([I 3; I 4]), ["y"; "2x"]); Val( I 5, "1x")], Primop(Plus, [(Primop(Plus, [Var "1x"; Var "y"])); Var "x"]))); 

  ]

(* Question 3a: evaluation *)

let rec evalList (exps : exp list) =
  List.map eval exps

and eval (exp : exp) : exp = match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
  appear during evaluation since they should be substituted away when
  eliminating binding constructs, e.g. function applications and lets.
  Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck ("Free variable (" ^ x ^ ") during evaluation"))

  (* primitive operations +, -, *, <, = *)
  | Primop (po, args) ->
      let argvalues = evalList args in
      (match evalOp(po, argvalues) with
       | None -> raise (Stuck "Bad arguments to primitive operation")
       | Some v -> v)

  | If (e, e1, e2) ->
      (match eval e with
       | B true -> eval e1
       | B false -> eval e2
       | _ -> raise (Stuck "Scrutinee of If is not true or false"))

  | Rec (f, _, e) -> eval (subst (exp, f) e)
                       
  | Apply (e1, e2) ->
      (match eval e1 with
       | Fn(x,_,e) -> eval (subst (e2,x) e)
       | _ -> raise (Stuck "Left term of application is not an Fn"))

  | Tuple es -> 
      Tuple (List.map (fun e -> eval e) es) 
      
  | Let ([], e2) -> eval e2
  
  | Let (dec1::decs, e2) ->
      (match dec1 with
       | Val(e1, x) ->
           eval (subst(eval e1, x) (Let(decs, e2))) 
       
       | Valtuple(e1, xs) -> match eval e1 with 
         | Tuple es -> eval (substList (List.combine es xs) 
                               (Let (decs, e2))) 
         | _ -> raise (Stuck "Something something not tuple")
      )

(* Question 3b: write your own tests for eval *)

let eval_tests : (exp, exp) tests =
  [ (Tuple [ I 3; I 3 ], Tuple [ I 3; I 3 ]);
    (Tuple [ I 4; I 3 ], Tuple [ I 4; I 3 ]);
    (Tuple [ I 5; I 3 ], Tuple [ I 5; I 3 ]);
    (Tuple [ I 6; I 3 ], Tuple [ I 6; I 3 ]);
    (Tuple [ I 7; I 3 ], Tuple [ I 7; I 3 ]);
    
  ]

(* Question 4a: type inference *)

let rec infer ctx e : tp = match e with
  | Var x -> (try lookup x ctx
              with NotFound -> raise (TypeError ("Found free variable")))
  | I _ -> Int
  | B _ -> Bool
  | Primop (po, exps) ->
      let (domain, range) =  primopType po in
      let rec check exps ts = match exps, ts with
        | [] , [] -> range
        | e::es , t::ts ->
            let t' = infer ctx e in
            if t' = t then check es test
            else type_mismatch t t'
      in
      check exps domain

  | If (e, e1, e2) ->
      (match infer ctx e with
       | Bool -> let t1 = infer ctx e1 in
           let t2 = infer ctx e2 in
           if t1 = t2 then t1
           else type_mismatch t1 t2
       | t -> type_mismatch Bool t)

  | Fn (x,t,e) -> Arrow (t, infer (extend ctx (x,t)) e)

  | Apply (e1, e2) -> (  
      let a = TVar (ref (None)) in 
      unify (Arrow ((infer ctx e2), a)) (infer ctx e1); a 
    ) 
  
  | Rec (f, t, e) -> let t1 = infer (extend ctx (f, t)) e in
      if t1 = t then t1 
      else type_mismatch t1 t 
                       
  | Tuple es -> Product (List.map (fun e -> infer ctx e) es) 
  
  | Let ([], e) -> infer ctx e 
                     
  | Let (dec::decs, e) ->
      let ctx' = infer_dec ctx dec in
      infer ctx' (Let (decs, e))

(** Extends the context with declarations made in Val or Valtuple. *)
and infer_dec ctx dec = match dec with
  | Val (e, x) -> extend ctx (x, infer ctx e)
  | Valtuple (e, nl) -> match infer ctx e with
    | Product ets -> extend_list ctx (List.combine nl ets)

(* Question 4b: write your own tests for infer *)

let infer_tests : (context * exp, tp) tests =
  [ ( ([], Tuple [ I 3; I 3 ])
    , Product [ Int; Int ]
    );
    
    ( ([], Tuple [ I 4; I 3 ])
    , Product [ Int; Int ]
    );
    
    ( ([], Tuple [ I 5; I 3 ])
    , Product [ Int; Int ]
    );
    
    ( ([], Tuple [ I 6; I 3 ])
    , Product [ Int; Int ]
    );
    
    ( ([], Tuple [ I 7; I 3 ])
    , Product [ Int; Int ]
    )
  ]
  