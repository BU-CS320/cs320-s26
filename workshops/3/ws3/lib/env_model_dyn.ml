
type expr = Ast.expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type value =
  | VNum of int
  | VFun of string * expr

module Env = Map.Make(String)
(* CHEATSHEET:
   ∅ ≡ Env.empty
   ℰ[x] ≡ Env.find x ℰ
   ℰ[x ↦ v] ≡ Env.add x v ℰ
*)

type dyn_env = value Env.t

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec eval (env : dyn_env) (e : expr) : value =
  match e with
  | Var y ->
    (*
        ℰ[x] = v
       ────────── (varE)
       ℰ ⊢ x ⇓ v
    *)
    Env.find y env
  | Num n ->
    (*
       n is an int lit
       ─────────────── (intLitE)
         ℰ ⊢ n ⇓ n
    *)
    VNum n
  | Fun (x, e) ->
    (*
       ───────────────────────── (funE)
       ℰ ⊢ fun x -> e ⇓ λ x . e
    *)
    VFun (x, e)
  | App (e1, e2) ->
    (*
       ℰ ⊢ e₁ ⇓ λ x . e      ℰ ⊢ e₂ ⇓ v₂      ℰ' = ℰ[x ↦ v₂]      ℰ' ⊢ e ⇓ v
       ───────────────────────────────────────────────────────────────────────── (appE)
                                   ℰ ⊢ e₁ e₂ ⇓ v
    *)
    begin
      match eval env e1 with
      | VFun (x, e) ->
        let env' = Env.add x (eval env e2) env in
        eval env' e
      | _ -> assert false
    end
  | Let (x, e1, e2) ->
    (*
       ℰ ⊢ e₁ ⇓ v₁      ℰ' = ℰ[x ↦ v₁]      ℰ' ⊢ e₂ ⇓ v
       ─────────────────────────────────────────────────── (letE)
                    ℰ ⊢ let x = e₁ in e₂ ⇓ v
    *)
    let v1 = eval env e1 in
    let env' = Env.add x v1 env in
    eval env' e2

let interp (s : string) : value option =
  match parse s with
  | Some expr ->
    begin
      match eval Env.empty expr with
      | v -> Some v
      | exception _ -> print_endline "Eval error"; None
    end
  | None -> print_endline "Parse error"; None
