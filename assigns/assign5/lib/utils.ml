
let all_opt (f : 'a -> 'b option) (l : 'a list) : 'b list option =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | x :: xs -> (
        match f x with
        | None -> None
        | Some x -> go (x :: acc) xs
      )
  in go [] l
