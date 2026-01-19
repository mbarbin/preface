module Nel = Preface.Nonempty_list

let nel_testable a =
  Alcotest.(testable (Nel.pp (pp a)) (Nel.equal (Alcotest.equal a)))
;;

let should_traverse_without_failure_app () =
  let open Nel.Applicative.Traversable (Preface.Option.Applicative) in
  let expected : _ Nel.t option = Some [ 1; 2; 3 ]
  and computed = sequence [ Some 1; Some 2; Some 3 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_traverse_without_failure" expected computed
;;

let should_traverse_with_failure_app () =
  let open Nel.Applicative.Traversable (Preface.Option.Applicative) in
  let expected = None
  and computed = sequence [ Some 1; None; Some 3 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_traverse_with_failure" expected computed
;;

let should_traverse_without_failure_monad () =
  let open Nel.Monad.Traversable (Preface.Option.Monad) in
  let expected : _ Nel.t option = Some [ 1; 2; 3 ]
  and computed = sequence [ Some 1; Some 2; Some 3 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_traverse_without_failure" expected computed
;;

let should_traverse_with_failure_monad () =
  let open Nel.Monad.Traversable (Preface.Option.Monad) in
  let expected = None
  and computed = sequence [ Some 1; None; Some 3 ] in
  Alcotest.(check (option (nel_testable int)))
    "should_traverse_with_failure" expected computed
;;

let cases =
  [
    ( "Nonempty_list"
    , let open Alcotest in
      [
        test_case
          "Traverse through nonempty list over option with success using \
           Applicative"
          `Quick should_traverse_without_failure_app
      ; test_case
          "Traverse through nonempty list over option with failure using \
           Applicative"
          `Quick should_traverse_with_failure_app
      ; test_case
          "Traverse through nonempty list over option with success using Monad"
          `Quick should_traverse_without_failure_monad
      ; test_case
          "Traverse through nonempty list over option with failure using Monad"
          `Quick should_traverse_with_failure_monad
      ] )
  ]
;;
