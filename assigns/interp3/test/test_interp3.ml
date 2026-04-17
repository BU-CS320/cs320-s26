open OUnit2
open Interp3


let type_of_tests =
  let _t expected input =
    assert_equal expected (type_of_expr Env.empty (parse_expr input))
  in
  "type_of tests" >:::
  [
    (* ADD MORE TESTS HERE *)
  ]

let eval_tests =
  let _t expected input =
    assert_equal expected (eval_expr Env.empty (parse_expr input))
  in
  "eval tests" >:::
  [
    (* ADD MORE TESTS HERE *)
  ]

let suite =
  "interp3 test suite" >:::
  [
    type_of_tests;
    eval_tests;
  ]

let _run = run_test_tt_main suite
