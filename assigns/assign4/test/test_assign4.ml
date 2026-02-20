open Assign4
open OUnit2

let expr_of_sexpr_opt_tests =
  let eq expected input =
    assert_equal expected (expr_of_sexpr_opt input)
  in
  "expr_of_sexpr_opt tests" >:::
  [
    "basic success" >::
    (fun _ ->
       eq
         (Some (Bop (Add, Bop (Mul, Int 1, Int 2), Int 3)))
         (List
            [Atom "+"
            ; List [Atom "*"; Atom "1"; Atom "2"]
            ; Atom "3"
            ]));
    "basic fail" >::
    (fun _ ->
       eq
         None
         (List
            [Atom "+"
            ; List [Atom "*"; Atom "1"; Atom "2"]
            ; Atom "3"
            ; Atom "4"
            ]));
    (* ADD MORE TESTS *)
  ]

let expr_of_string_opt_tests =
  let eq expected input =
    assert_equal expected (expr_of_string_opt input)
  in
  "expr_of_string_opt tests" >:::
  [
    "basic success" >::
    (fun _ ->
       eq
         (Some (If (Int 2, Int 3, Int 4)))
         "(if 2 3 4)");
    "basic fail" >::
    (fun _ ->
       eq
         None
         "(if (2) 3 4)");
    (* ADD MORE TESTS *)
  ]

let string_of_expr_tests =
  let eq expected input =
    assert_equal expected (string_of_expr input)
  in
  "string_of_expr tests" >:::
  [
    "basic" >::
    (fun _ ->
       eq
         "(+ (= 2 3) (= 4 5))"
         (Bop
            ( Add
            , Bop (Eq, Int 2, Int 3)
            , Bop (Eq, Int 4, Int 5)
            )));
    (* ADD MORE TESTS *)
  ]

let ty_deriv_of_string_opt_tests =
  let eq expected input =
    assert_equal expected (ty_deriv_of_string_opt input)
  in
  "ty_deriv_of_string_opt tests" >:::
  [
    "basic success" >::
    (fun _ ->
       eq
         (Some
            (Rule_app
               {
                 prem_derivs =
                   [
                     Rule_app
                       {
                         prem_derivs = [];
                         concl = { expr = Int 1; ty = IntT };
                         rname = Int_lit;
                       };
                     Rule_app
                       {
                         prem_derivs = [];
                         concl = { expr = Int 2; ty = IntT };
                         rname = Int_lit;
                       };
                   ];
                 concl = { expr = Bop (Add, Int 1, Int 2); ty = IntT };
                 rname = Add_int;
               }))
         "((+ 1 2) int ADDINT (1 int INTLIT) (2 int INTLIT))");
    "basic with holes" >::
    (fun _ ->
       eq
         (Some
            (Rule_app
               {
                 prem_derivs =
                   [
                     Hole;
                     Rule_app
                       {
                         prem_derivs = [];
                         concl = { expr = Int 1; ty = IntT };
                         rname = Int_lit;
                       };
                     Hole;
                   ];
                 concl = { expr = If (Bop (Eq, Int 0, Int 1), Int 1, Int 2); ty = IntT };
                 rname = If_rule;
               }))
         "((if (= 0 1) 1 2) int IF ??? (1 int INTLIT) ???)");
    "basic failure" >::
    (fun _ ->
       eq
         None
         "((+ 1 2) ADDINT int ??? ???)")
  ]

let check_rule_tests =
  let eq expected (rname, prems, concl) =
    assert_equal expected (check_rule rname prems concl)
  in
  "check_rule tests" >:::
  [
    "basic intLit" >::
    (fun _ ->
       eq
         true
         ( Int_lit
         , []
         , { expr = Int 2; ty = IntT }
         ));
    "basic addInt" >::
    (fun _ ->
       eq
         true
         ( Add_int
         , [
           Some { expr = Int 2 ; ty = IntT };
           Some { expr = Int 3 ; ty = IntT };
         ]
         , { expr = Bop (Add, Int 2, Int 3); ty = IntT }
         ));
    "basic addInt with hole" >::
    (fun _ ->
       eq
         true
         ( Add_int
         , [
           Some { expr = Int 2 ; ty = IntT };
           None
         ]
         , { expr = Bop (Add, Int 2, Int 3); ty = IntT }
         ));
    "basic addInt fail" >::
    (fun _ ->
       eq
         false
         ( Add_int
         , [
           Some { expr = Int 2 ; ty = BoolT };
           None
         ]
         , { expr = Bop (Add, Int 2, Int 3); ty = IntT }
         ));
    (* ADD MORE TESTS *)
  ]

let check_deriv_tests =
  let eq expected input =
    assert_equal
      expected
      (check_deriv
         (Option.get (ty_deriv_of_string_opt input)))
  in
  "check_deriv tests" >:::
  [
    "basic intLit" >::
    (fun _ ->
       eq
         Complete
         "(2 int INTLIT)");
    "basic addInt" >::
    (fun _ ->
       eq
         Complete
         "((+ 1 2) int ADDINT (1 int INTLIT) (2 int INTLIT))");
    "basic addInt with hole" >::
    (fun _ ->
       eq
         Partial
         "((+ 1 2) int ADDINT ??? (2 int INTLIT))");
    "basic addInt failure" >::
    (fun _ ->
       eq
         Invalid
         "((+ 1 2) int ADDINT ??? (2 int INTLIT) ???)");
    (* ADD MORE TESTS *)
  ]

let value_of_expr_tests =
  let eq expected input =
    assert_equal expected (value_of_expr input)
  in
  "value_of_expr tests" >:::
  [
    "basic addInt" >::
    (fun _ ->
       eq
         (IntV 5)
         (Bop (Add, Int 2, Int 3)));
    "basic if" >::
    (fun _ ->
       eq
         (BoolV true)
         (If
            ( Bop (Eq, Int 0, Int 1)
            , Bop (Eq, Int 0, Int 1)
            , Bop (Eq, Int 1, Int 1)
            )));
    (* ADD MORE TESTS *)
  ]

let suite =
  "assignment 4 test suite" >:::
  [
    expr_of_sexpr_opt_tests;
    expr_of_string_opt_tests;
    string_of_expr_tests;
    ty_deriv_of_string_opt_tests;
    check_rule_tests;
    check_deriv_tests;
    value_of_expr_tests;
  ]

let _ = run_test_tt_main suite
