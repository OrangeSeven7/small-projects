open Util
open Worker_manager


let m1 = Mutex.create ();;
let m2 = Mutex.create ();;
let a1 = Mutex.create ();;
let r1 = Mutex.create ();;
let r2 = Mutex.create ();;
let c1 = Mutex.create ();;
let c2 = Mutex.create ();;




let kv_p : (string * string) list ref = ref []

let kvs_p : (string * string list) list ref = ref []

let mnm : (string * string) list ref = ref []

let rnr : (string * (string list)) list ref = ref []

let fails_map :  ('a, 'b) Hashtbl.t ref = ref (Hashtbl.create 512)

let fails_red : ('a, 'b) Hashtbl.t ref = ref (Hashtbl.create 512)

let combine_tbl : ('a, 'b) Hashtbl.t ref = ref (Hashtbl.create 512)

let wrkrmn : Worker_manager.mapper Worker_manager.worker_manager list ref = ref []

let wrkrrn : Worker_manager.reducer Worker_manager.worker_manager list ref = ref []

let update_table (x,y) = Mutex.lock c1;
  if Hashtbl.mem !combine_tbl x
  then ((Hashtbl.replace !combine_tbl x (y::(Hashtbl.find !combine_tbl x))); (Mutex.unlock c1))
  else (Hashtbl.add !combine_tbl x [y]); Mutex.unlock c1

let update_fails_map x = Mutex.lock m1;
  if Hashtbl.mem !fails_map x
  then (Hashtbl.replace !fails_map x ((Hashtbl.find !fails_map x) + 1);  Mutex.unlock m1)
  else Hashtbl.add !fails_map x 0;  Mutex.unlock m1

let update_fails_red x = Mutex.lock r1;
  if Hashtbl.mem !fails_red x
  then (Hashtbl.replace !fails_red x ((Hashtbl.find !fails_red x) + 1);  Mutex.unlock r1)
  else Hashtbl.add !fails_red x 0;  Mutex.unlock r1

let desome x =
  match x with
  |Some y -> y
  |None -> failwith "Nothing"

let create_kv k =
  kv_p := k

let create_kvs k =
  kvs_p := k

let add_map () = Mutex.lock m1;
try
  let x = Worker_manager.pop_worker (List.hd !wrkrmn) in
  let z = (match !kv_p with
    |(h1,h2)::t -> if (Hashtbl.find !fails_map h1) >10 then kv_p := t else
    (match Worker_manager.map x h1 h2 with
      |Some y -> (kv_p := t); (mnm := y@(!mnm))
      |None -> update_fails_map h1)
    |[] -> ()) in
    Worker_manager.push_worker (List.hd !wrkrmn) x; Mutex.unlock m1
  with _-> Mutex.unlock m1


let rec add_work_m r k = Mutex.lock a1;
  print_endline "a";
  match k with
    |h::t -> let x = (Thread_pool.add_work add_map r) in print_endline "b"; (Mutex.unlock a1); add_work_m r t; print_endline "b"
    |[] -> print_endline "b2"; (Mutex.unlock a1); ()



let rec combine_helper_1 v = Mutex.lock c1;
  print_endline "s";
  match v with
    |(h1,h2)::t -> update_table (h1,h2); print_endline "1.1"; Mutex.unlock c1; combine_helper_1 t ; print_endline "1.2"
    |[] ->  print_endline "1.3"

let combine_helper_2 k v_list acc =
  print_endline "s2";
  (k, v_list) :: acc


let add_reduce () =
  Mutex.lock r1;
  try
  let x = Worker_manager.pop_worker (List.hd !wrkrrn) in
  let z = (match !kvs_p with
    |(h1,h2)::t -> if (Hashtbl.find !fails_red h1) >10 then kvs_p := t else
    (match Worker_manager.reduce x h1 h2 with
      |Some y -> (kvs_p := t); (rnr := (h1,y)::(!rnr))
      |None -> update_fails_red h1)
    |[] -> ()) in
    Worker_manager.push_worker (List.hd !wrkrrn) x; Mutex.unlock r1
  with _-> Mutex.unlock r1





let rec add_work_r r k = Mutex.lock a1;
  print_endline "d";
  match k with
    |h::t -> let x = (Thread_pool.add_work add_reduce r); Mutex.unlock a1 in add_work_r r k
    |[] -> (Mutex.unlock a1); ()



(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let r = create_kv kv_pairs in
  print_endline ("/n" ^ string_of_int (List.length !kv_p));
  let w = Worker_manager.initialize_mappers map_filename in
  let res = (wrkrmn := [w]) in
  let g = Thread_pool.create 20 in
  let x = add_work_m g !kv_p; print_endline "done" in
  let masdfa = Thread_pool.destroy g in
    !mnm

let combine kv_pairs : (string * string list) list = 
  let x = combine_helper_1 kv_pairs in
  Hashtbl.fold combine_helper_2 !combine_tbl []


let reduce kvs_pairs reduce_filename : (string * string list) list =
  let r = create_kvs kvs_pairs in print_endline "7.1"; 
  let w  = Worker_manager.initialize_reducers reduce_filename in print_endline "7.2"; 
  let res = (wrkrrn := [w]) in print_endline "7.3"; 
  let g = Thread_pool.create 20 in print_endline "7.4"; 
  let x = add_work_r g !kvs_p in print_endline "7.5";
  let masdfa = Thread_pool.destroy g in print_endline "7.6";
  let afasd = Worker_manager.clean_up_workers w in
    !rnr





let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced