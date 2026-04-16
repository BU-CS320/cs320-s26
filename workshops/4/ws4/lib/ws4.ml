
type ty =
  | Int
  | Bool
  | Fun of ty * ty
  | Param of string

type constr = ty * ty
type solution = (string * ty) list

let unify (cs : constr list) : solution option =
  ignore cs; assert false

let apply (s : solution) (t : ty) : ty =
  ignore (s, t); assert false
