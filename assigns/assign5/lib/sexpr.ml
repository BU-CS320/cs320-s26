
type 'a t =
  | Atom of 'a
  | List of 'a t list

let is_lower c = let c = int_of_char c in 97 <= c && c <= 122
let is_upper c = let c = int_of_char c in 65 <= c && c <= 90
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

let rec sexpr_of_tokens_opt (ts : token list) : (string t * token list) option =
  match ts with
  | [] -> None
  | t :: ts -> (
      match t with
      | Word a -> Some (Atom a, ts)
      | Lpar -> (
          match sexprs_of_tokens_opt ts with
          | es, Rpar :: ts -> Some (List es, ts)
          | _ -> None
        )
      | Rpar -> None
    )
and sexprs_of_tokens_opt (ts : token list) : string t list * token list =
  match sexpr_of_tokens_opt ts with
  | Some (e, ts) ->
    let (es, ts) = sexprs_of_tokens_opt ts in
    e :: es, ts
  | None -> [], ts

let list_of_tokens_opt ts =
  match sexprs_of_tokens_opt ts with
  | e, [] -> Some e
  | _ -> None

let of_tokens_opt (ts : token list) : string t option =
  match sexpr_of_tokens_opt ts with
  | Some (e, []) -> Some e
  | _ -> None

let of_string_opt (s : string) : string t option =
  of_tokens_opt (tokens_of_string s)

let list_of_string_opt (s : string) : string t list option =
  list_of_tokens_opt (tokens_of_string s)
