
type pos = Lexing.position * Lexing.position
let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos

let _gensym =
  let count = ref 0 in
  fun () ->
    count := !count + 1;
    "$" ^ string_of_int !count

let map_ok f =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match f x with
        | Ok x -> go (x :: acc) xs
        | Error e -> Error e
      )
  in go []
