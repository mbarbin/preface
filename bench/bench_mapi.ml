(* Benchmark: mapi implementations for nonempty lists *)

type 'a nel = ( :: ) of 'a * 'a list

(* Current implementation: pattern match on List.mapi result *)
let mapi_current f (x :: xs) =
  match List.mapi f List.(x :: xs) with
  | List.[] -> assert false
  | List.(y :: ys) -> y :: ys
;;

(* Alternative: closure with index adjustment *)
let mapi_closure f (x :: xs) = f 0 x :: List.mapi (fun i e -> f (succ i) e) xs

(* Create a nonempty list of given size *)
let make_nel n =
  let rec aux acc i = if i <= 0 then acc else aux List.(i :: acc) (i - 1) in
  match aux List.[] n with
  | List.[] -> failwith "n must be >= 1"
  | List.(x :: xs) -> x :: xs
;;

(* Timing utility *)
let time_it name f n_iters =
  Gc.full_major ();
  let start = Unix.gettimeofday () in
  for _ = 1 to n_iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf
    "%s: %.4f s (%d iters, %.2f ns/iter)\n%!"
    name
    elapsed
    n_iters
    (elapsed /. float n_iters *. 1e9)
;;

(* The function to map *)
let f i x = i + x

let () =
  let sizes = List.[ 1; 5; 10; 50; 100; 1000 ] in
  let n_iters = 1_000_000 in
  List.iter
    (fun size ->
      Printf.printf "\n=== List size: %d ===\n" size;
      let nel = make_nel size in
      let iters = n_iters / size in
      (* scale iterations inversely with size *)
      time_it "current (pattern match)" (fun () -> mapi_current f nel) iters;
      time_it "closure (succ i)      " (fun () -> mapi_closure f nel) iters)
    sizes
;;
