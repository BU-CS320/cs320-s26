let usage = "USAGE: dune exec -- interp1 <filename>"

let () =
  if Array.length Sys.argv <> 2
  then print_endline usage
  else ignore (Interp1.interp ~filename:Sys.argv.(1))
