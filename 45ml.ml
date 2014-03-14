#load "melodytree.cmo"
open Melodytree;;



(* Determines*)
let f (x: note)  (accum: melody): melody=
  match accum with
      [] -> (match (x mod 12) with
              |9 -> 1::(x-1)::[]
              |4 -> 1::(x-1)::[]
              |11 -> 1::(x-1)::[]
              |0 -> 0::x::[]
              |i -> 1::x::[])
    |h::t -> 
        match h with
          |9 -> (match (x mod 12) with
                  |9 -> 9::x::t
                  |4 -> 1::(x-1)::t
                  |11 -> 1::(x-1)::t
                  |0 -> 0::x::t
                  |i -> 1::x::t)
          |11 -> (match (x mod 12) with
                   |9 -> 9::x::t
                   |4 -> 1::(x-1)::t
                   |11 -> 11::x::t
                   |0 -> 0::x::t
                   |i -> 1::x::t)
          |0 -> (match (x mod 12) with
                  |9 -> 1::(x-1)::t
                  |4 -> 1::(x-1)::t
                  |11 -> 11::x::t
                  |0 -> 0::x::t
                  |i -> 1::x::t)
          |1 -> (match (x mod 12) with
                  |9 -> 1::(x-1)::t
                  |4 -> 1::(x-1)::t
                  |11 -> 1::(x-1)::t
                  |0 -> 0::x::t
                  |i -> 1::x::t)





let major_to_melodic_minor (r: melody) : melody =
  match List.fold_right f r [] with
      [] -> []
    |h::t -> t;;







let rec comb (accum: node list) (x: node) =
  if is_goal x then x::accum
  else List.fold_left comb accum (children x);;

let x (y: node) (z:node) =
  if (badness y) > (badness z) then 1 else if (badness y) < (badness z) then -1 else 0;;

let find_best_counterpoint_naive (y: bassline) (z: mode) : melody option =
  match List.sort x (List.rev (comb [] (start (y,z)))) with
      [] -> None
    |hd::tl -> Some(counterpoint hd);;

find_best_counterpoint_naive [2;5;7;5;4;2] Ionian;;




let rec choose (x:node) (y:node): node = 
  if (is_goal x) then
    (if (badness x) > (badness y) then y else x)
  else List.fold_left choose y (children x)



let find_best_counterpoint_guided (y: bassline) (z: mode) : melody option =
  match start (y,z) with
      x -> Some (counterpoint (choose (List.hd(children x)) x)) ;;


find_best_counterpoint_guided  [2;5;7;5;4;2] Ionian;;

