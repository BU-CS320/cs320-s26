type expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
