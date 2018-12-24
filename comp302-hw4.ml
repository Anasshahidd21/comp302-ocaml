(* -------------------------------------------------------------*)
(* QUESTION 1 : Let's have cake!                                *)
(* -------------------------------------------------------------*)

(* allergy_free : ingredient list -> cupcake list -> cupcake list *)
(* let allergy_free allergens cupcakes =
  let rec get_allergy_free allergens cupcakes = match cupcakes with 
    | Cupcake (pr, wgt, clr, ingredients) :: cupcakes' -> 
        if List.for_all (fun i -> not (List.mem i allergens)) ingredients 
        then Cupcake (pr, wgt, clr, ingredients)  :: (get_allergy_free allergens cupcakes')
        else get_allergy_free allergens cupcakes' 	
    | _ -> []
  in
  get_allergy_free allergens cupcakes
 *)

let allergy_free allergens cupcakes = 
	List.filter (fun (Cupcake (pr, wgt, clr, ingredients)) 
    -> List.for_all (fun i -> 
      not (List.exists (fun a -> a = i) allergens)) ingredients) cupcakes

(* -------------------------------------------------------------*)
(* QUESTION 2 : Generic Tree Traversals                         *)
(* -------------------------------------------------------------*)

(* map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t = match t with
	| Empty -> Empty
 	| Node (a, l, r) -> Node ((f a), (map_tree f l), (map_tree f r))

(* delete_data : ('a * 'b) tree -> 'a tree *)
let delete_data t =
  let del (key, value) = key in
  map_tree del t

(* fold_tree : ('a * 'b ' * 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t = match t with
 	| Empty -> e
	| Node (a, l, r) -> f (a, (fold_tree f e l), (fold_tree f e r))	

(* size : 'a tree -> int *)
let size tr =
 	fold_tree (fun (a, l, r) -> 1 + l + r) 0 tr

(* reflect : 'a tree -> 'a tree *)
let reflect tr =
 	fold_tree (fun (a, l, r) -> Node (a, r, l)) Empty tr

(* inorder : 'a tree -> 'a list *)
let inorder tr =
 	fold_tree (fun (a, l, r) -> l @ [a] @ r) [] tr
