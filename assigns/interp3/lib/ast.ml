open Utils
module VarSet = Set.Make(String)

module Type = struct
  type t =
    | TUnit
    | TBool
    | TInt
    | TString
    | TTuple of t list
    | TAdt of t list * string
    | TFun of t * t
    | TParam of string

  let rec pp ppf ty =
    let open Fmt in
    let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | TAdt (_ :: _, _) -> parens pp ppf ty
    | _ -> pp ppf ty
    in
    match ty with
    | TUnit -> pf ppf "unit"
    | TBool -> pf ppf "bool"
    | TInt -> pf ppf "int"
    | TString -> pf ppf "string"
    | TParam a -> string ppf a
    | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp t2
    | TTuple ts -> list ~sep:(Fmt.any " * ") pp ppf ts
    | TAdt ([], n) -> pf ppf "%s" n
    | TAdt ([t1], n) -> pf ppf "%a %s" pp_parens t1 n
    | TAdt (ts, n) -> pf ppf "%a %s" (parens (list ~sep:comma pp)) ts n

  (* TODO: REMOVE THESE *)
  let mk _pos ty = ty
  let unit pos = mk pos TUnit
  let bool pos = mk pos TBool
  let int pos = mk pos TInt
  let string pos = mk pos TString
  let tuple pos ts = mk pos (TTuple ts)
  let param pos a = mk pos (TParam a)
  let adt pos name args = mk pos (TAdt (name, args))
  let fun_ pos t1 t2 = mk pos (TFun (t1, t2))
end

module Pattern = struct
  type pattern =
    | PWild
    | PVar of string
    | PUnit
    | PBool of bool
    | PInt of int
    | PString of string
    | PTuple of t list
    | PCons of string * t option
  and t =
    {
      pos : pos;
      pattern : pattern;
    }

  let mk pos pattern = {pos; pattern}

  let wild pos = mk pos PWild
  let var pos x = mk pos (PVar x)
  let unit pos = mk pos PUnit
  let true_ pos = mk pos (PBool true)
  let false_ pos = mk pos (PBool false)
  let int pos n = mk pos (PInt n)
  let string pos s = mk pos (PString s)
  let tuple pos ps = mk pos (PTuple ps)
  let cons pos c args = mk pos (PCons (c, args))
  let list_lit pos ps =
    match ps with
    | [] -> cons pos "Nil" None
    | e :: es ->
      let rest =
        List.fold_right
          (fun e es -> cons dummy_pos "Cons" (Some (tuple dummy_pos [e;es])))
          es
          (cons pos "Nil" None)
      in
      cons pos "Cons" (Some (tuple dummy_pos [e; rest]))
end

module Expr = struct
  type bop =
    | Add | Sub | Mul
    | Div | Mod
    | And | Or
    | Concat
    | Eq | Neq | Lt | Lte | Gt | Gte

  type expr =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Negate of t
    | Bop of bop * t * t
    | If of t * t * t
    | Annot of t * Type.t
    | Tuple of t list
    | Assert of t
    | Var of string
    | Cons of string * t option
    | Fun of (string * Type.t option) * t
    | App of t * t
    | Let of
        {
          is_rec : bool;
          name : string;
          binding : t;
          body : t;
        }
    | Match of t * (Pattern.t * t) list
  and t =
    {
      pos : pos;
      expr : expr;
    }

  let mk pos expr = {pos; expr}
  let unit pos = mk pos Unit
  let true_ pos = mk pos (Bool true)
  let false_ pos = mk pos (Bool false)
  let int pos n = mk pos (Int n)
  let string pos s = mk pos (String s)
  let negate pos e = mk pos (Negate e)
  let bop pos e1 op e2 = mk pos (Bop (op, e1, e2))
  let if_ pos e1 e2 e3 = mk pos (If (e1, e2, e3))
  let annot pos x ty = mk pos (Annot (x, ty))
  let tuple pos tys = mk pos (Tuple tys)
  let assert_ pos e = mk pos (Assert e)
  let var pos x = mk pos (Var x)
  let cons pos c arg = mk pos (Cons (c, arg))
  let list_lit pos es =
    match es with
    | [] -> cons pos "Nil" None
    | e :: es ->
      let rest =
        List.fold_right
          (fun e es -> cons dummy_pos "Cons" (Some (tuple dummy_pos [e;es])))
          es
          (cons pos "Nil" None)
      in
      cons pos "Cons" (Some (tuple dummy_pos [e; rest]))
  let fun_ ?annot_ty pos (args : (string * Type.t option) list) body =
    let body =
      match annot_ty with
      | None -> body
      | Some ty -> annot dummy_pos body ty
    in
    match args with
    | [] -> body
    | arg :: args ->
      let e = List.fold_right (fun arg acc -> mk dummy_pos (Fun (arg, acc))) args body in
      mk pos (Fun (arg, e))
  let let_ pos is_rec name args annot_ty binding body =
    let binding = fun_ ?annot_ty dummy_pos args binding in
    mk pos (Let {is_rec; name; binding; body})
  let match_ pos es cs = mk pos (Match (es, cs))
  let app pos f arg = mk pos (App (f, arg))
end

module Stmt = struct
  type stmt =
    | SLet of
        {
          is_rec : bool;
          name : string;
          binding : Expr.t;
        }

    | SAdt of
        {
          tpars : string list;
          name : string;
          constrs : (string * Type.t option) list
        }
  and t =
    {
      pos : pos;
      stmt : stmt;
    }

  let mk pos stmt = {pos; stmt}

  let let_ pos is_rec name args annot_ty binding =
    let binding = Expr.fun_ ?annot_ty dummy_pos args binding in
    mk pos (SLet {is_rec; name; binding})
  let adt pos tpars name constrs =
    mk pos (SAdt {tpars; name; constrs})
end

type prog = Stmt.t list
