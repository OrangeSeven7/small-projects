type 'a tree   = Leaf of 'a | Node of int * 'a tree * 'a tree
type 'a tbit   = Zero | One of 'a tree
type 'a ralist = 'a tbit list

(****************************** Tree Helpers **********************************)

let rec depth (x: 'a tree) : int =
  match x with
    |Node (g,h,k) -> 1 + depth h + depth k
    |Leaf (j) -> 1

let size_tree (t: 'a tree) : int =
  depth t;;

(*--------------------------------------------------------------*)

let merge (t: 'a tree) (t': 'a tree): 'a tree  = 
  Node ((size_tree t + size_tree t'), t, t');;

(*--------------------------------------------------------------*)

let rec kth (n: int) (t: 'a tree) : 'a =
  match t with
      Leaf r -> r
    |Node (g,h,i) -> if n> size_tree h then kth (n-g) i
        else kth n h


let look_tree (n: int) (t: 'a tree)   =
  if size_tree t < n then failwith "fewer than k nodes"
  else kth n t

(*--------------------------------------------------------------*)

let rec kth_rep(n: int) (v: 'a) (t: 'a tree) =
  match t with
      Leaf r -> Leaf v
    |Node (g,h,i) -> if n> size_tree h then Node(g,h,kth_rep (n-g) v i)
        else Node(g,kth_rep (n) v h, i)


let update_tree (n: int) (v: 'a) (t: 'a tree) = 
  if size_tree t < n then failwith "fewer than k nodes"
  else kth_rep n v t

(*--------------------------------------------------------------*)

let rec inc_helper (v: 'a tree) (xs: 'a ralist) =
  match xs with
      [] -> [One v]
    |h::t -> match h with
      |Zero -> (One v)::t
      |One k -> Zero::(inc_helper (merge v k) t);;

let increment_tree (v: 'a) (xs: 'a ralist) : 'a ralist  = 
  inc_helper (Leaf v) xs;;


(*--------------------------------------------------------------*)

let rec leaves_to_list n (k: 'a tree) =
  if n <= size_tree k then (look_tree n k):: leaves_to_list (n+1) k else [];;

let rec help (xr: 'a ralist) (xs: 'a ralist) =
  match xs with
      [] -> failwith "empty"
    |h::t ->
        match h with
          |One k -> ((kth 1 k),List.fold_right increment_tree (leaves_to_list 1 k) t)
          |Zero -> help (xr @ [Zero]) t


let decrement_tree (xs: 'a ralist) =
  help [] xs;;



(****************************** RAList Functions ******************************)

let empty = 
  [];;
(*--------------------------------------------------------------*)

let rec is_empty (g: 'a ralist): bool  = 
  match g with
    |[] -> true
    |h::t -> if h = Zero then is_empty t else false;;

(*--------------------------------------------------------------*)

let cons  (g: 'a) (k: 'a ralist): 'a ralist=
  increment_tree g k;;

(*--------------------------------------------------------------*)


let tail (i: 'a ralist): 'a ralist=
  match decrement_tree i with
    |(h,t) -> t

(*--------------------------------------------------------------*)

let rec to_list (i: 'a ralist): 'a list  =
  match i with
    |[] -> []
    |h::t ->
        match h with
          |Zero -> to_list t
          |One r -> (leaves_to_list 1 r) @ (to_list t)


(*--------------------------------------------------------------*)

let rec from_list (k: 'a list) : 'a ralist =
  match k with
    |[] -> empty
    |h::t -> increment_tree h (from_list t)


(*--------------------------------------------------------------*)

let rec head (i: 'a ralist): 'a =
  match decrement_tree i with
    |(h,t) -> h

(*--------------------------------------------------------------*)


let lookup (i: int) (j: 'a ralist): 'a =
  List.nth (to_list j) i;;


(*--------------------------------------------------------------*)
let rec pow (k: int) (j: int) : int =
  if j = 0 then 1 else k * pow k (j-1);;


let rec find_num (i: int) (j: int): int=
  if (pow 2 i) <= j then find_num (i+1) j else i-1;;



let rec find_tree (k: int) (i: int) (j: 'a) (r: 'a ralist): 'a ralist =
  match r with
    |[] -> []
    |h::t -> if i > 0 then h:: (find_tree k (i-1) j t) else
          match h with
            |Zero -> failwith "internal mismatch error"
            |One y -> (One (kth_rep k j y))::t


let update (i: int) (j: 'a) (k: 'a ralist)   = 
  find_tree (i - (pow 2 (find_num 0 i))) (find_num 0 i) j k













