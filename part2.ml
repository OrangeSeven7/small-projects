type bignum = int64 list;;

let f (x: int64)  (accum: bignum)=
  match accum with
      [] -> if Int64.shift_right x 16 > 0L then (Int64.shift_right x 16)::[Int64.logand x 0xffffL] else [x]
    |h::t -> (Int64.shift_right (Int64.add x h) 16)::(Int64.logand (Int64.add x h) 0xffffL)::t;;


let f2 (x: int64) (y: int64) (accum: bignum) =
  (Int64.add x y)::accum;;

let f3 (x: int64) (y: int64) (accum: bignum) =
  (Int64.mul x y)::accum;;



let canonicalize (r:bignum): bignum =
  match List.fold_right f (0L::0L::r) [] with
    |h::h2::h3::t -> if h>0L then h::h2::h3::t else
          (if h2 >0L then h2::h3::t else 
             (if h3 >0L then h3::t else t));;


let rec alter (x:bignum) (y:int) : bignum =
  if List.length x < y then alter (0L::x) y else x;;

let bignum_plus (x:bignum) (y:bignum): bignum =
  canonicalize(List.fold_right2 f2 (alter x (List.length y)) (alter y (List.length x)) []);;

(*multiplies bignums*)
let bignum_mult (x:bignum) (y:bignum): bignum =
  canonicalize(List.fold_right2 f3 x y []);;


