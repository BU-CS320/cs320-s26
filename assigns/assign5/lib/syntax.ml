open Utils
open Sexpr

type op = Add | Mul

type expr =
  | Ident of string * string list
  | Map of op * expr * expr
  | Fold of op * string * expr

type stmt =
  | Init of string * int list * float Sexpr.t
  | Set of string * string list * expr

let atom_opt (e : string Sexpr.t) : string option =
  match e with
  | Atom x -> Some x
  | _ -> None

let shape_of_sexpr_opt (e : string Sexpr.t) : int list option =
  match e with
  | Atom _ -> None
  | List shape ->
    match all_opt atom_opt shape with
    | None -> None
    | Some shape -> all_opt int_of_string_opt shape

let op_of_sexpr_opt (e : string Sexpr.t) : op option =
  match e with
  | Atom op -> (
    match op with
      | "+" -> Some Add
      | "*" -> Some Mul
      | _ -> None
  )
  | _ -> None

let axis_labels_of_sexpr_opt (e : string Sexpr.t) : string list option =
  match e with
  | List labels -> all_opt atom_opt labels
  | _ -> None

let rec expr_of_sexpr_opt (e : string Sexpr.t) : expr option =
  match e with
  | List [Atom a; idxs] ->
    Option.map (fun x -> Ident (a, x)) (axis_labels_of_sexpr_opt idxs)
  | List [Atom "MAP"; op; e1; e2] -> (
      match op_of_sexpr_opt op, expr_of_sexpr_opt e1, expr_of_sexpr_opt e2 with
      | Some op, Some e1, Some e2 -> Some (Map (op, e1, e2))
      | _ -> None
    )
  | List [Atom "FOLD"; op; Atom idx; expr] -> (
      match op_of_sexpr_opt op, expr_of_sexpr_opt expr with
      | Some op, Some expr -> Some (Fold (op, idx, expr))
      | _ -> None
    )
  | _ -> None

let rec float_of_string_sexpr_opt (e : string Sexpr.t) : float Sexpr.t option =
  match e with
  | Atom f -> Option.map (fun x -> Atom x) (float_of_string_opt f)
  | List es ->
    es
    |> all_opt float_of_string_sexpr_opt
    |> Option.map (fun x -> List x)

let stmt_of_sexpr_opt (e : string Sexpr.t) : stmt option =
  match e with
  | List [Atom "INIT"; Atom a; shape; sexpr] -> (
    match shape_of_sexpr_opt shape, float_of_string_sexpr_opt sexpr with
    | Some shape, Some sexpr -> Some (Init (a, shape, sexpr))
    | _ -> None
  )
  | List [Atom "SET"; Atom a; labels; sexpr] -> (
    match axis_labels_of_sexpr_opt labels, expr_of_sexpr_opt sexpr with
    | Some labels, Some expr -> Some (Set (a, labels, expr))
    | _ -> None
  )
  | _ -> None
