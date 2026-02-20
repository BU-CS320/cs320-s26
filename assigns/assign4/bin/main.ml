
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

let process_repl user_input () =
  let open Assign4 in
  match interp user_input with
  | Ok (Rule_app deriv, Some value) ->
    print_endline
      (String.concat "\n"
         [
           "";
           string_of_ty_deriv (Rule_app deriv);
           "";
           Printf.sprintf "- : %s = %s"
             (string_of_ty deriv.concl.ty)
             (string_of_value value)
         ])
  | Ok (deriv, None) ->
    print_endline
      (String.concat "\n"
         [
           "";
           string_of_ty_deriv deriv;
           "";
           "- : ??? = ???"
         ])
  | Error (Invalid_deriv deriv) ->
    print_endline
      (String.concat "\n"
         [
           "";
           string_of_ty_deriv deriv;
           "";
           "ERROR: invalid derivation!"
         ])
  | Error Parse_error -> print_endline "ERROR: parse error!"
  | _ -> assert false
  | exception _ -> print_endline "whoops!"

let process_file = process_repl

let welcome_msg = "ArithDeriv version 0.0.1\nEnter #quit;; to exit\n"
let usage_str = "usage: dune exec assign4 [filename]"

let () =
  match Array.length Sys.argv with
  | 1 ->
    repl ~welcome_msg () process_repl
  | 2 -> (
    let filename = Sys.argv.(1) in
    match read_file filename with
    | user_input -> process_file user_input ()
    | exception _ -> Printf.printf "error: could not read '%s'\n" filename
  )
  | _ -> print_endline usage_str
