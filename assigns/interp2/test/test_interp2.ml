open OUnit2
open Interp2


let type_of_tests =
  let t expected input =
    assert_equal expected (type_of_expr Env.empty (parse_expr input))
  in
  "type_of tests" >:::
  [
    "unit" >::
    (fun _ -> t
        (Ok TUnit)
        "()");
    "fun" >::
    (fun _ -> t
        (Ok (TFun (TInt, TInt)))
        ("fun (x : int) -> x"));
    (* ADD MORE TESTS HERE *)
  ]

let eval_tests =
  let t expected input =
    assert_equal expected (eval_expr Env.empty (parse_expr input))
  in
  "eval tests" >:::
  [
    "unit" >::
    (fun _ -> t
        VUnit
        "()");
    "let+fun" >::
    (fun _ -> t
        (VInt 2)
        "let x = 2 in (fun (y : bool) -> x) true");
    (* ADD MORE TESTS HERE *)
  ]

let suite =
  "interp1 test suite" >:::
  [
    type_of_tests;
    eval_tests;
  ]

let _run = run_test_tt_main suite
