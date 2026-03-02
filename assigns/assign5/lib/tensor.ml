open Utils

type entries =
  | Num of float
  | Array of entries Array.t

type t = {
  idx_space : (string * int) list;
  entries : entries
}

let idx_space (t : t) : (string * int) list = t.idx_space
let shape (t : t) : int list = List.map (fun (_, n) -> n) (idx_space t)
let axis_labels t : string list = List.map (fun (n, _) -> n) (idx_space t)

let relabel_assoc (l : ('a * 'b) list) (js : 'a list) : ('a * 'b) list =
  let rec go acc js l =
    match js, l with
    | [], [] -> List.rev acc
    | x :: xs, (_, z) :: ys -> go ((x, z) :: acc) xs ys
    | _ -> assert false
  in go [] js l

let relabel_axes (t : t) (labels : string list) =
  { t with idx_space = relabel_assoc (idx_space t) labels }

exception Out_of_bounds

let get (t : t) (idx : (string * int) list) : float =
  let rec go entries idx =
    match entries, idx with
    | Num f, []  -> f
    | Array entries, i :: idxs -> go entries.(i) idxs
    | _ -> raise Out_of_bounds
  in
  go
    t.entries
    (List.map (fun n -> List.assoc n idx) (axis_labels t))

let init (idx_space : (string * int) list) (entries : (string * int) list -> float) : t =
  let rec go idx_space entries =
    match idx_space with
    | [] -> Num (entries [])
    | (label, d) :: ds ->
      let mk i = go ds (fun idx -> entries ((label, i) :: idx)) in
      let entries = Array.init d mk in
      Array entries
  in
  let entries = go idx_space entries in
  {idx_space; entries}

let of_sexpr_opt (shape : int list) (entries : float Sexpr.t) : t option =
  let rec go shape entries =
    match shape, entries with
    | [], Sexpr.Atom f -> Some (Num f)
    | d :: ds, List ts ->
      if List.length ts <> d
      then None
      else
        ts
        |> all_opt (go ds)
        |> Option.map Array.of_list
        |> Option.map (fun x -> Array x)
    | _ -> None
  in
  let idx_space = List.mapi (fun i x -> (string_of_int i, x)) shape in
  entries
  |> go shape
  |> Option.map (fun x -> {idx_space;entries=x})

let to_pretty (t : t) : string =
  let rec go t =
    match t.entries with
    | Num f -> [string_of_float f]
    | Array entries ->
      let (idx_label, _) = List.hd (idx_space t) in
      let idx_space = List.tl (idx_space t) in
      ("idx: " ^ idx_label) :: go' [] idx_space (Array.to_list entries)
  and go' has_lines idx_space ts =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_lines
    in
    match ts with
    | [] -> []
    | [Num f] -> [lines ^ "└──" ^ (string_of_float f)]
    | Num f :: es ->
      let next_line = lines ^ "├──" ^ (string_of_float f) in
      next_line :: go' has_lines [] es
    | [Array entries] ->
      let (idx_label, _) = List.hd idx_space in
      let idx_space = List.tl idx_space in
      let next_line = lines ^ "└──" ^ "idx: " ^ idx_label in
      next_line :: go' (false :: has_lines) idx_space (Array.to_list entries)
    | Array entries :: ts ->
      let (n, _) = List.hd idx_space in
      let next_line = lines ^ "├──" ^ "idx: " ^ n in
      next_line :: go' (true :: has_lines) (List.tl idx_space) (Array.to_list entries) @ go' has_lines idx_space ts
  in String.concat "\n" (go t)
