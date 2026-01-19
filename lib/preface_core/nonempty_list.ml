type 'a t = ( :: ) of 'a * 'a list

let create x = x :: []

let rev_append (x :: xs) (y :: ys) =
  match List.rev_append List.(x :: xs) List.(y :: ys) with
  | [] -> assert false
  | List.(z :: zs) -> z :: zs
;;

let hd (x :: _) = x
let tl = function _ :: [] -> None | _ :: y :: ys -> Some (y :: ys)

let rev (x :: xs) =
  match List.(rev (x :: xs)) with
  | [] -> assert false
  | List.(y :: ys) -> y :: ys
;;

let from_list = function [] -> None | List.(x :: xs) -> Some (x :: xs)
let to_list (x :: xs) = List.(x :: xs)
let length (_ :: xs) = 1 + List.length xs
let cons x (y :: ys) = x :: List.(y :: ys)
let iteri f (x :: xs) = List.iteri f List.(x :: xs)
let iter f (x :: xs) = List.iter f List.(x :: xs)

let mapi f (x :: xs) =
  match List.mapi f List.(x :: xs) with
  | [] -> assert false
  | List.(y :: ys) -> y :: ys
;;

let map f (x :: xs) =
  match List.map f List.(x :: xs) with
  | [] -> assert false
  | List.(y :: ys) -> y :: ys
;;

let fold_left f acc (x :: xs) = List.fold_left f acc List.(x :: xs)
let reduce f (x :: xs) = List.fold_left f x xs
let fold_right f (x :: xs) acc = List.fold_right f List.(x :: xs) acc
let append (x :: xs) (y :: ys) = x :: List.append xs List.(y :: ys)
let flatten nel = reduce append nel

let equal f (x :: xs) (y :: ys) =
  f x y
  &&
  let rec aux = function
    | [], [] -> true
    | List.(a :: as'), List.(b :: bs') -> f a b && aux (as', bs')
    | _ -> false
  in
  aux (xs, ys)
;;

let pp pp' formater list =
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.(fprintf formater "@[[%a]@]" (pp_print_list ~pp_sep pp') (to_list list))
;;
