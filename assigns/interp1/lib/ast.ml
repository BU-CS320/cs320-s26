open Utils
module VarSet = Set.Make(String)

module Type = struct
  type ty =
    | Unit
    | Bool
    | Int
    | String
    | List of t
    | Param of string
    | Fun of t * t
    | Adt of t list * string
  and t =
    {
      pos : pos;
      ty : ty;
    }

  let rec pp ppf ty =
    let open Fmt in
    let pp_parens ppf ty =
    match ty.ty with
    | List _
    | Fun (_, _)
    | Adt (_ :: _, _) -> parens pp ppf ty
    | _ -> pp ppf ty
    in
    match ty.ty with
    | Unit -> pf ppf "unit"
    | Bool -> pf ppf "bool"
    | Int -> pf ppf "int"
    | String -> pf ppf "string"
    | List ty -> pf ppf "%a list" pp_parens ty
    | Param a -> string ppf a
    | Fun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp t2
    | Adt ([], n) -> pf ppf "%s" n
    | Adt ([t1], n) -> pf ppf "%a %s" pp_parens t1 n
    | Adt (ts, n) -> pf ppf "%a %s" (parens (list ~sep:comma pp)) ts n

  let mk pos ty = {pos; ty}
  let unit pos = mk pos Unit
  let bool pos = mk pos Bool
  let int pos = mk pos Int
  let string pos = mk pos String
  let list pos t = mk pos (List t)
  let param pos a = mk pos (Param a)
  let fun_ pos t1 t2 = mk pos (Fun (t1, t2))
  let adt pos name args = mk pos (Adt (name, args))

  type 'a scheme = VarSet.t * ty
end

module Pattern = struct
  type pattern =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Cons of t * t
    | List_lit of t list
    | Constr of string * t list
    | Var of string
  and t =
    {
      pos : pos;
      pattern : pattern;
    }

  let rec pp ppf p =
    let open Fmt in
    let pp_parens ppf p =
      match p.pattern with
      | Cons (_, _)
      | Constr (_, _::_) -> parens pp ppf p
      | _ -> pp ppf p
    in
    match p.pattern with
    | Unit -> pf ppf "()"
    | Bool b -> bool ppf b
    | Int n -> int ppf n
    | String s -> quote string ppf s
    | Cons (p, ps) -> pf ppf "%a :: %a" pp_parens p pp ps
    | List_lit ps -> brackets (list ~sep:semi pp) ppf ps
    | Var x -> string ppf x
    | Constr (n, []) -> string ppf n
    | Constr (n, [p]) -> pf ppf "%s %a" n pp_parens p
    | Constr (n, ps) -> pf ppf "%s %a" n (parens (list ~sep:comma pp)) ps

  let mk pos pattern = {pos; pattern}

  let unit pos = mk pos Unit
  let true_ pos = mk pos (Bool true)
  let false_ pos = mk pos (Bool false)
  let int pos n = mk pos (Int n)
  let string pos s = mk pos (String s)
  let cons pos x xs = mk pos (Cons (x, xs))
  let list_lit pos l = mk pos (List_lit l)
  let var pos x = mk pos (Var x)
  let constr pos c args = mk pos (Constr (c, args))

end

module Expr = struct
  type bop =
    | Add | Sub | Mul | Div | Mod
    | Eq | Neq | Lt | Lte | Gt | Gte
    | And | Or
    | Cons | Concat

  type expr =
    | Let of
        {
          is_rec : bool;
          name : string;
          args : (string * Type.t option) list;
          annot : Type.t option;
          binding : t;
          body : t;
        }
    | If of t * t * t
    | Fun of (string * Type.t option) list * t
    | Match of t list * (Pattern.t list * t) list
    | Bop of bop * t * t
    | Assert of t
    | Negate of t
    | Print_endline of t
    | Constr of string * t list
    | App of t * t list
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | List_lit of t list
    | Var of string
    | Annot of t * Type.t
  and t =
    {
      pos : pos;
      expr : expr;
    }

  let mk pos expr = {pos; expr}

  let let_ pos is_rec name args annot binding body =
    mk pos (Let {is_rec; name; args; annot; binding; body})
  let if_ pos e1 e2 e3 = mk pos (If (e1, e2, e3))
  let fun_ pos args body = mk pos (Fun (args, body))

  let match_ pos es cs = mk pos (Match (es, cs))

  let bop pos e1 op e2 = mk pos (Bop (op, e1, e2))
  let assert_ pos e = mk pos (Assert e)
  let constr pos c args = mk pos (Constr (c, args))
  let app pos f args = mk pos (App (f, args))

  let unit pos = mk pos Unit
  let true_ pos = mk pos (Bool true)
  let false_ pos = mk pos (Bool false)
  let int pos n = mk pos (Int n)
  let string pos s = mk pos (String s)
  let list_lit pos es = mk pos (List_lit es)
  let var pos x = mk pos (Var x)
  let annot pos x ty = mk pos (Annot (x, ty))
  let print pos e = mk pos (Print_endline e)
  let negate pos e = mk pos (Negate e)
end

module Stmt = struct
  type stmt =
    | Let of
        {
          is_rec : bool;
          name : string;
          args : (string * Type.t option) list;
          annot : Type.t option;
          binding : Expr.t;
        }

    | Adt of
        {
          tpars : string list;
          name : string;
          constrs : (string * Type.t list) list
        }
  and t =
    {
      pos : pos;
      stmt : stmt;
    }

  let mk pos stmt = {pos; stmt}

  let let_ pos is_rec name args annot binding =
    mk pos (Let {is_rec; name; args; annot; binding})
  let adt pos tpars name constrs =
    mk pos (Adt {tpars; name; constrs})
end

type prog = Stmt.t list

module Interp1 = struct
  let ( let* ) = Result.bind

  let not_implemented pos =
    Error (Error_msg.mk pos "invalid syntax")

  type ty =
    | Unit
    | Bool
    | Int
    | Fun of ty * ty

  let ty_of_ty (t : Type.t) : (ty, Error_msg.t) result =
    let rec go (ty : Type.t) : (ty, Error_msg.t) result =
      match ty.ty with
      | Unit -> Ok Unit
      | Bool -> Ok Bool
      | Int -> Ok Int
      | Fun (t1, t2) ->
        let* t1 = go t1 in
        let* t2 = go t2 in
        Ok (Fun (t1, t2))
      | _ -> not_implemented ty.pos
    in go t

  type bop =
    | Add | Sub | Mul | Div | Mod
    | Eq | Neq | Lt | Lte | Gt | Gte
    | And | Or

  let bop_of_bop (pos : pos) (op : Expr.bop) : (bop, Error_msg.t) result =
    match op with
    | Add -> Ok Add
    | Sub -> Ok Sub
    | Mul -> Ok Mul
    | Div -> Ok Div
    | Mod -> Ok Mod
    | Eq -> Ok Eq
    | Neq -> Ok Neq
    | Lt -> Ok Lt
    | Lte -> Ok Lte
    | Gt -> Ok Gt
    | Gte -> Ok Gte
    | And -> Ok And
    | Or -> Ok Or
    | _ -> not_implemented pos

  type expr =
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

  let expr_of_expr (e : Expr.t) : (expr, Error_msg.t) result =
    let rec go (expr : Expr.t) : (expr, Error_msg.t) result =
      match expr.expr with
      | Unit -> Ok Unit
      | Bool b -> Ok (Bool b)
      | Int n -> Ok (Int n)
      | Var x -> Ok (Var x)
      | Let data -> (
          match data.is_rec, data.args, data.annot with
          | false, [], None ->
            let* binding = go data.binding in
            let* body = go data.body in
            Ok (Let (data.name, binding, body))
          | true, [(arg, Some arg_ty)], Some out_ty ->
            let name = data.name in
            let* arg_ty = ty_of_ty arg_ty in
            let* out_ty = ty_of_ty out_ty in
            let* binding = go data.binding in
            let* body = go data.body in
            Ok (LetRec {name; arg; arg_ty; out_ty; binding; body})
          | _ -> not_implemented expr.pos
        )
      | If (e1, e2, e3) ->
        let* e1 = go e1 in
        let* e2 = go e2 in
        let* e3 = go e3 in
        Ok (If (e1, e2, e3))
      | Fun ([x, Some ty], body) ->
        let* ty = ty_of_ty ty in
        let* body = go body in
        Ok (Fun (x, ty, body))
      | Bop (op, e1, e2) ->
        let* op = bop_of_bop expr.pos op in
        let* e1 = go e1 in
        let* e2 = go e2 in
        Ok (Bop (op, e1, e2))
      | App (e, args) ->
        let rec process acc (es : Expr.t list) =
          match es with
          | [] -> Ok acc
          | e :: es -> (
              match go e with
              | Ok e -> process (App (acc, e)) es
              | Error e -> Error e
            )
        in
        let* e = go e in
        process e args
      | Negate e ->
        let* e = go e in
        Ok (Negate e)
      | Assert e ->
        let* e = go e in
        Ok (Assert e)
      | _ -> not_implemented expr.pos
    in go e

  let expr_of_prog (p : Stmt.t list) : (expr, Error_msg.t) result =
    let go (prog : Stmt.t list) : (expr, Error_msg.t) result =
      match prog with
      | [stmt] -> (
          match stmt.stmt with
          | Let data -> (
              match data.is_rec, data.args, data.annot with
              | false, [], None ->
                let* e = expr_of_expr data.binding in
                Ok e
              | _ -> not_implemented stmt.pos
            )
          | _ -> not_implemented dummy_pos
        )
      | _ -> not_implemented dummy_pos
    in go p
end
