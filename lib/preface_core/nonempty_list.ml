type 'a t = ( :: ) of 'a * 'a list

let create x = x :: []

let rev_append (x :: xs) (y :: ys) =
  match List.rev_append xs (x :: y :: ys) with
  | [] -> assert false
  | z :: zs -> z :: zs
;;

let hd (x :: _) = x
let tl (_ :: xs) = match xs with [] -> None | y :: ys -> Some (y :: ys)

let rev (x :: xs) =
  match List.rev (x :: xs) with [] -> assert false | y :: ys -> y :: ys
;;

let from_list = function [] -> None | x :: xs -> Some (x :: xs)
let to_list (x :: xs) = List.(x :: xs)
let length (_ :: xs) = 1 + List.length xs
let cons x (y :: ys) = x :: y :: ys
let iteri f (x :: xs) = List.iteri f (x :: xs)

let iter f (x :: xs) =
  f x;
  List.iter f xs
;;

let mapi f (x :: xs) =
  match List.mapi f (x :: xs) with [] -> assert false | y :: ys -> y :: ys
;;

let map f (x :: xs) =
  let y = f x in
  y :: List.map f xs
;;

let fold_left f acc (x :: xs) = List.fold_left f (f acc x) xs
let reduce f (x :: xs) = List.fold_left f x xs
let fold_right f (x :: xs) acc = List.fold_right f (x :: xs) acc
let append (x :: xs) (y :: ys) = x :: List.append xs (y :: ys)

let[@tail_mod_cons] rec flatten_list_of_nels = function
  | [] -> []
  | (x :: xs) :: ys -> x :: append_to_nels xs ys

and[@tail_mod_cons] append_to_nels xs ys =
  match xs with
  | [] -> flatten_list_of_nels ys
  | x :: xs -> x :: append_to_nels xs ys
;;

let flatten ((x :: xs) :: ys) = x :: append_to_nels xs ys
let equal f (x :: xs) (y :: ys) = f x y && List.equal f xs ys

let pp pp' formater list =
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.(fprintf formater "@[[%a]@]" (pp_print_list ~pp_sep pp') (to_list list))
;;
