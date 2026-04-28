let () =
  match Sys.argv.(1) with
  | "compile" ->
    Sys.argv.(2)
    |> open_in
    |> In_channel.input_all
    |> Demo.parse
    |> Demo.compile
  | "interp" ->
    Sys.argv.(2)
    |> open_in
    |> In_channel.input_all
    |> Demo.parse
    |> Demo.eval Demo.Env.empty
    |> Demo.string_of_value
    |> print_endline
  | _ -> print_endline "usage: dune exec demo <interp | compile> <filename>"
