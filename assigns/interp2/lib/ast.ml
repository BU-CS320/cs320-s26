open Utils
module VarSet = Set.Make(String)

module Type = struct
  type ty =
    | Unit
    | Bool
    | Int
    | String
    | Param of string
    | Fun of t * t
    | Tuple of t list
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
    | Fun (_, _)
    | Tuple _
    | Adt (_ :: _, _) -> parens pp ppf ty
    | _ -> pp ppf ty
    in
    match ty.ty with
    | Unit -> pf ppf "unit"
    | Bool -> pf ppf "bool"
    | Int -> pf ppf "int"
    | String -> pf ppf "string"
    | Param a -> string ppf a
    | Fun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp t2
    | Tuple ts -> list ~sep:(Fmt.any "*@ ") pp ppf ts
    | Adt ([], n) -> pf ppf "%s" n
    | Adt ([t1], n) -> pf ppf "%a %s" pp_parens t1 n
    | Adt (ts, n) -> pf ppf "%a %s" (parens (list ~sep:comma pp)) ts n

  let mk pos ty = {pos; ty}
  let unit pos = mk pos Unit
  let bool pos = mk pos Bool
  let int pos = mk pos Int
  let string pos = mk pos String
  let param pos a = mk pos (Param a)
  let fun_ pos t1 t2 = mk pos (Fun (t1, t2))
  let tuple pos ts = mk pos (Tuple ts)
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
    | Tuple of t list
    | Constr of string * t option
    | Var of string
  and t =
    {
      pos : pos;
      pattern : pattern;
    }

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
  let tuple pos ps = mk pos (Tuple ps)
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
    | Match of t * (Pattern.t * t) list
    | Bop of bop * t * t
    | Assert of t
    | Negate of t
    | Tuple of t list
    | Constr of string * t option
    | App of t * t list
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Print_endline
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
  let negate pos e = mk pos (Negate e)
  let print pos = mk pos Print_endline
  let constr pos c arg = mk pos (Constr (c, arg))
  let app pos f args = mk pos (App (f, args))

  let unit pos = mk pos Unit
  let true_ pos = mk pos (Bool true)
  let false_ pos = mk pos (Bool false)
  let int pos n = mk pos (Int n)
  let string pos s = mk pos (String s)
  let list_lit pos es = mk pos (List_lit es)
  let var pos x = mk pos (Var x)
  let annot pos x ty = mk pos (Annot (x, ty))
  let tuple pos tys = mk pos (Tuple tys)
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
          constrs : (string * Type.t option) list
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

module Interp2 = struct
  let ( let* ) = Result.bind

  let not_implemented pos =
    Error (Error_msg.mk pos "invalid syntax")

  type ty =
    | TUnit
    | TBool
    | TInt
    | TInt_list
    | TFun of ty * ty
    | TTuple of ty list

  let ty_of_ty (t : Type.t) : (ty, Error_msg.t) result =
    let rec go ({pos;ty} : Type.t) : (ty, Error_msg.t) result =
      let* ty = _go {pos;ty} in
      Ok ty
    and _go (ty : Type.t) : (ty, Error_msg.t) result =
      match ty.ty with
      | Unit -> Ok TUnit
      | Bool -> Ok TBool
      | Int -> Ok TInt
      | Adt ([inner_ty], "list") -> (
          match go inner_ty with
          | Ok TInt -> Ok TInt_list
          | Ok _ -> not_implemented ty.pos
          | Error err -> Error err
        )
      | Fun (t1, t2) ->
        let* t1 = go t1 in
        let* t2 = go t2 in
        Ok (TFun (t1, t2))
      | Tuple ts ->
        let* ts = map_ok go ts in
        Ok (TTuple ts)
      | _ -> not_implemented ty.pos
    in go t

  type _pattern =
    | PUnit
    | PBool of bool
    | PInt of int
    | PNil
    | PCons of pattern * pattern
    | PTuple of pattern list
    | PVar of string
  and pattern =
    {
      pos : pos;
      pattern : _pattern;
    }

  let pattern_of_pattern (p : Pattern.t) : (pattern, Error_msg.t) result =
    let rec go ({pos;pattern} : Pattern.t) : (pattern, Error_msg.t) result =
      let* pattern = _go {pos;pattern} in
      Ok {pos; pattern}
    and _go (p : Pattern.t) : (_pattern, Error_msg.t) result =
      match p.pattern with
      | Unit -> Ok PUnit
      | Bool b -> Ok (PBool b)
      | Int n -> Ok (PInt n)
      | List_lit [] -> Ok PNil
      | Cons (x, xs) ->
        let* x = go x in
        let* xs = go xs in
        Ok (PCons (x, xs))
      | Tuple ps ->
        let* ps = map_ok go ps in
        Ok (PTuple ps)
      | Var x -> Ok (PVar x)
      | _ -> not_implemented p.pos
    in
    go p

  type bop =
    | Add | Sub | Mul | Div | Mod
    | Eq | Neq | Lt | Lte | Gt | Gte
    | And | Or | Cons

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
    | Cons -> Ok Cons
    | Concat -> not_implemented pos

  type _expr =
    | Unit
    | Bool of bool
    | Int of int
    | Var of string
    | Nil
    | Assert of expr
    | Negate of expr
    | Tuple of expr list
    | Bop of bop * expr * expr
    | If of expr * expr * expr
    | Fun of (string * ty) list * expr
    | App of expr * expr list
    | Let of
        {
          is_rec : bool;
          name : string;
          args : (string * ty) list;
          annot : ty option;
          binding : expr;
          body : expr;
        }
    | Match of expr * (pattern * expr) list
  and expr =
    {
      pos : pos;
      expr : _expr;
    }

  let annot_of_annot annot =
    match annot with
    | Some ty -> Result.map (fun x -> Some x) (ty_of_ty ty)
    | None -> Ok None

  let args_of_args pos =
    map_ok
      (fun (x, ty) ->
         match ty with
         | None -> not_implemented pos
         | Some ty ->
           Result.map
             (fun ty -> (x, ty))
             (ty_of_ty ty))

  let rec arms_of_arms arms =
    map_ok
      (fun (p, e) ->
         let* p = pattern_of_pattern p in
         let* e = expr_of_expr e in
         Ok (p, e))
      arms

  and expr_of_expr (e : Expr.t) : (expr, Error_msg.t) result =
    let rec go ({pos;expr} : Expr.t) : (expr, Error_msg.t) result =
      let* expr = _go {pos;expr} in
      Ok {pos; expr}
    and _go (e : Expr.t) : (_expr, Error_msg.t) result =
      match e.expr with
      | Unit -> Ok Unit
      | Bool b -> Ok (Bool b)
      | Int n -> Ok (Int n)
      | Var x -> Ok (Var x)
      | Assert e ->
        let* e = go e in
        Ok (Assert e)
      | Negate e ->
        let* e = go e in
        Ok (Negate e)
      | Tuple es ->
        let* es = map_ok go es in
        Ok (Tuple es)
      | Bop (op, e1, e2) ->
        let* op = bop_of_bop e.pos op in
        let* e1 = go e1 in
        let* e2 = go e2 in
        Ok (Bop (op, e1, e2))
      | If (e1, e2, e3) ->
        let* e1 = go e1 in
        let* e2 = go e2 in
        let* e3 = go e3 in
        Ok (If (e1, e2, e3))
      | Fun (args, body) ->
        let* args = args_of_args e.pos args in
        let* body = go body in
        Ok (Fun (args, body))
      | App (e, es) ->
        let* e = go e in
        let* es = map_ok go es in
        Ok (App (e, es))
      | Let {is_rec;name;args;annot;binding;body} ->
        let* args = args_of_args e.pos args in
        let* annot = annot_of_annot annot in
        let* binding = go binding in
        let* body = go body in
        Ok (Let {is_rec;name;args;annot;binding;body})
      | Match (e, arms) ->
        let* e = go e in
        let* arms = arms_of_arms arms in
        Ok (Match (e, arms))
      | List_lit [] -> Ok Nil
      | _ -> not_implemented e.pos
    in
    go e

  type _stmt =
    | SLet of
        {
          is_rec : bool;
          name : string;
          args : (string * ty) list;
          annot : ty option;
          binding : expr;
        }
  and stmt =
    {
      pos : pos;
      stmt : _stmt;
    }

  let stmt_of_stmt (s : Stmt.t) : (stmt, Error_msg.t) result =
    let rec go ({pos;stmt} : Stmt.t) : (stmt, Error_msg.t) result =
      let* stmt = _go {pos;stmt} in
      Ok {pos;stmt}
    and _go (s : Stmt.t) : (_stmt, Error_msg.t) result =
      match s.stmt with
      | Let {is_rec;name;args;annot;binding} ->
        let* args = args_of_args s.pos args in
        let* annot = annot_of_annot annot in
        let* binding = expr_of_expr binding in
        Ok (SLet {is_rec;name;args;annot;binding})
      | _ -> not_implemented s.pos
    in go s

  type prog = stmt list

  let prog_of_prog = map_ok stmt_of_stmt
end
