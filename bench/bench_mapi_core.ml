(* Benchmark: mapi implementations using Core_bench *)

open Core
open Core_bench

module Nel : sig
  type 'a t

  val make : int -> int t
  val mapi_current : (int -> 'a -> 'b) -> 'a t -> 'b t
  val mapi_closure : (int -> 'a -> 'b) -> 'a t -> 'b t
end = struct
  type 'a t = ( :: ) of 'a * 'a list

  let mapi_current f (x :: xs) =
    match Stdlib.List.mapi f Stdlib.List.(x :: xs) with
    | Stdlib.List.[] -> assert false
    | Stdlib.List.(y :: ys) -> y :: ys
  ;;

  let mapi_closure f (x :: xs) =
    f 0 x :: Stdlib.List.mapi (fun i e -> f (succ i) e) xs
  ;;

  let make n =
    let rec aux acc i =
      if i <= 0 then acc else aux Stdlib.List.(i :: acc) (i - 1)
    in
    match aux Stdlib.List.[] n with
    | Stdlib.List.[] -> failwith "n must be >= 1"
    | Stdlib.List.(x :: xs) -> x :: xs
  ;;
end

let f i x = i + x

let () =
  let sizes = [ 1; 5; 10; 50; 100; 1000 ] in
  let benchmarks =
    List.concat_map sizes ~f:(fun size ->
      let nel = Nel.make size in
      [ Bench.Test.create ~name:(sprintf "current (size=%d)" size) (fun () ->
          ignore (Sys.opaque_identity (Nel.mapi_current f nel)))
      ; Bench.Test.create ~name:(sprintf "closure (size=%d)" size) (fun () ->
          ignore (Sys.opaque_identity (Nel.mapi_closure f nel)))
      ])
  in
  Command_unix.run (Bench.make_command benchmarks)
;;
