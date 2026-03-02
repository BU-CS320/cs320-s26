open OUnit2
open Assign5

let a =
  let idx_space = [("i", 3); ("j", 2)] in
  let mk idx =
    let i = List.assoc "i" idx in
    let j = List.assoc "j" idx in
    float_of_int i *. 2. +. float_of_int j
  in
  Tensor.init idx_space mk

let b =
  let idx_space = [("i", 2); ("j", 3)] in
  let mk idx =
    let i = List.assoc "i" idx in
    let j = List.assoc "j" idx in
    float_of_int i  +. 2. *. float_of_int j
  in
  Tensor.init idx_space mk

let c = Tensor.init [] (fun _ -> 15.)

let d =
  let idx_space = [("l", 2); ("m", 2)] in
  let mk idx =
    let i = List.assoc "l" idx in
    let j = List.assoc "m" idx in
    match i, j with
    | 0, 0 -> 20.
    | 0, 1 -> 26.
    | 1, 0 -> 26.
    | 1, 1 -> 35.
    | _ -> assert false
  in
  Tensor.init idx_space mk

let suite =
  "assign5 test suite" >:::
  [
    "transpose" >:: (fun _ ->
        assert_equal
          (Assign5.interp [("A", a)] "(SET B (i j) (A (j i)))")
          (Ok [("B", b); ("A", a)]));
    "sum" >:: (fun _ ->
        assert_equal
          (Assign5.interp [("A", a)] "(SET C () (FOLD + i (FOLD + j (A (i j)))))")
          (Ok [("C", c); ("A", a)]));
    "A.T @ A" >:: (fun _ ->
        assert_equal
          (Assign5.interp [("B", b); ("A", a)] "(SET D (l m) (FOLD + k (MAP * (B (l k)) (A (k m)))))")
          (Ok [("D", d); ("B", b); ("A", a)]));
    (* ADD MORE TESTS *)
  ]

let _ = run_test_tt_main suite
