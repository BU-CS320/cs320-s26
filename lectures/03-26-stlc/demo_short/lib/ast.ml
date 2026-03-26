type ty =
  | Int
  | TFun of ty * ty

type expr =
  | Var of string
  | Num of int
  | Fun of string * ty * expr
  | App of expr * expr
  | Let of string * expr * expr
