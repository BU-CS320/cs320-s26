open Assign5

let single_input (prompt : string) : string =
  let open String in
  let rec go first_line input =
    let prompt =
      if first_line
      then prompt ^ " "
      else make (length prompt + 1) ' '
    in
    let _ = print_string prompt in
    let next_line = read_line () |> trim in
    if ends_with ~suffix:";;" next_line
    then
      let next_line = sub next_line 0 (length next_line -2) in
      trim (input ^ "\n" ^ next_line)
    else go false (input ^ "\n" ^ next_line)
  in go true ""

let read_file filename =
  let chan = open_in filename in
  let rec go acc =
    match input_line chan with
    | exception End_of_file -> acc
    | line -> go (acc ^ "\n" ^ line)
  in go ""

let repl
    ?(welcome_msg="")
    ?(prompt="#")
    ?(exit_directive="#quit")
    (start : 'a)
    (process : string -> 'a -> 'a) : unit =
  let rec go env () =
    let user_input = single_input prompt in
    if user_input = exit_directive
    then exit 0
    else
      go (process user_input env) ()
  in
  print_endline welcome_msg;
  go start ()

let process_repl user_input env =
  match Assign5.interp env user_input with
  | Ok ((a, t) :: env) ->
    let _ = print_endline ("\n" ^ Tensor.to_pretty t ^ "\n") in
    (a, t) :: env
  | Ok [] -> assert false
  | Error Parse_error -> print_endline "error: parse error"; env
  | Error Init_error -> print_endline "error: ill-formed tensor initialization"; env
  | Error Dim_error -> print_endline "error: dimensionality error"; env

let process_file user_input env =
  let _ = process_repl user_input env in
  ()

let welcome_msg = "TensorCalc version 0.0.1\nEnter #quit;; to exit\n"
let usage_str = "usage: dune exec assign5 [filename]"

let () =
  match Array.length Sys.argv with
  | 1 ->
    repl ~welcome_msg [] process_repl
  | 2 -> (
    let filename = Sys.argv.(1) in
    match read_file filename with
    | user_input -> process_file user_input [];
    | exception _ -> Printf.printf "error: could not read '%s'\n" filename
  )
  | _ -> print_endline usage_str
