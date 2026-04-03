
type pos = Lexing.position * Lexing.position
let dummy_pos = Lexing.dummy_pos, Lexing.dummy_pos

let map_ok f =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match f x with
        | Ok x -> go (x :: acc) xs
        | Error e -> Error e
      )
  in go []

let combine_opt l r =
  let rec go acc l r =
    match l, r with
    | x :: xs, y :: ys -> go ((x, y) :: acc) xs ys
    | [], [] -> Some (List.rev acc)
    | _ -> None
  in go [] l r

let fold_left_opt f base l =
  let ( let* ) = Option.bind in
  let rec go acc l =
    match l with
    | [] -> Some acc
    | x :: xs ->
      let* acc = f acc x in
      go acc xs
  in go base l
