open OUnit2
open Interp1

let type_of_tests =
  let t expected input =
    assert_equal expected (type_of Env.empty input)
  in
  "type_of tests" >:::
  [
    "unit" >::
    (fun _ -> t
        (Some Unit)
        Unit);
    "fun" >::
    (fun _ -> t
        (Some (Fun (Int, Int)))
        (Fun ("x", Int, Var "x")));
    (* ADD MORE TESTS HERE *)
  ]

let eval_tests =
  let t expected input =
    assert_equal expected (eval Env.empty input)
  in
  "eval tests" >:::
  [
    "unit" >::
    (fun _ -> t
        Unit
        Unit);
    "let+fun" >::
    (fun _ -> t
      (Clos (Env.(add "x" (Int 2) empty), None, Fun ("y", Bool, Var "x")))
      (Let ("x", Int 2, Fun ("y", Bool, Var "x"))));
    (* ADD MORE TESTS HERE *)
  ]

let suite =
  "interp1 test suite" >:::
  [
    type_of_tests;
    eval_tests;
  ]

let _run = run_test_tt_main suite
