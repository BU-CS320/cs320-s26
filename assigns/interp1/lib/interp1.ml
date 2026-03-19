
(* Syntax *)

type ty = Ast.Interp1.ty =
  | Unit
  | Bool
  | Int
  | Fun of ty * ty

type bop = Ast.Interp1.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type expr = Ast.Interp1.expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | LetRec of {
      name : string;
      arg : string;
      arg_ty : ty;
      out_ty : ty;
      binding : expr;
      body : expr;
    }
  | If of expr * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | Negate of expr
  | Assert of expr

(* Environments *)

module Env = Map.Make (String)

(* Values *)

type value =
  | Unit
  | Bool of bool
  | Int of int
  | Clos of value Env.t * string option * expr

(* Contexts *)

type ctxt = ty Env.t

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Type Checking *)

let type_of (ctxt : ctxt) (e : expr) : ty option =
  ignore (ctxt, e); assert false (* TODO *)

(* Evaluation *)

exception Div_by_zero
exception Assert_fail

let eval (env : dyn_env) (e : expr) : value =
  ignore (env, e); assert false (* TODO *)

(* Interpretation *)

let interp ~(filename : string) : value option =
  let e_ty =
    match Syntax.parse ~filename with
    | Ok p -> Ast.Interp1.expr_of_prog p
    | Error e -> Error e
  in
  match e_ty with
  | Ok e -> (
      match type_of Env.empty e with
      | Some _ -> Some (eval Env.empty e)
      | _ ->
        let _type_error_msg = print_endline "Type error"
        in None
    )
  | Error e ->
    let _parse_error_msg =
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Error_msg.to_string ~filename ~text e in
           Format.eprintf "%s" msg)
    in None
