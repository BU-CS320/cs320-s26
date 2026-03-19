
type pos = Lexing.position * Lexing.position
let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos

let gensym =
  let count = ref 0 in
  fun () ->
    count := !count + 1;
    "$" ^ string_of_int !count
