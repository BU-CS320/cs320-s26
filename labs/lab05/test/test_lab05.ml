open Lab05
open OUnit2

let sexpr_of_string_opt_tests =
  "sexpr_of_string_opt tests" >:::
  [
    "atom only" >::
    (fun _ ->
       assert_equal
         (Some (Atom "abc"))
         (sexpr_of_string_opt "abc"));
    "list only" >::
    (fun _ ->
       assert_equal
         (Some (List [Atom "abc"; Atom "def"; Atom "ghi"]))
         (sexpr_of_string_opt "  (  abc  def  ghi  )\n"));
    (* ADD MORE TESTS *)
  ]

let suite =
  "S-expression parser test suite" >:::
  [
    sexpr_of_string_opt_tests
  ]

let _ = run_test_tt_main suite
