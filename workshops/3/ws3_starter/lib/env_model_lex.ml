
type expr = Ast.expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr

module Env = Map.Make(String)
(* CHEATSHEET:
   ∅ ≡ Env.empty
   ℰ[x] ≡ Env.find x ℰ
   ℰ[x ↦ v] ≡ Env.add x v ℰ
*)

type value =
  | VNum of int
  | VBool of bool
  | VClos of value Env.t * expr

type dyn_env = value Env.t

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec eval (env : dyn_env) (e : expr) : value =
  match e with
  | Var x ->
    (*
        ℰ[x] = v
       ────────── (varE)
       ℰ ⊢ x ⇓ v
    *)
    ignore (env, x); assert false (* TODO *)
  | Num n ->
    (*
       n is an int lit
       ─────────────── (intLitE)
         ℰ ⊢ n ⇓ n
    *)
    ignore n; assert false (* TODO *)
  | Fun (x, e) ->
    (*
       ────────────────────────────────────── (funE)
       ℰ ⊢ fun x → e ⇓ ⦇ ℰ , fun x → e ⦈
    *)
    ignore (x, e); assert false (* TODO *)
  | App (e1, e2) ->
    (*
       ℰ₁ ⊢ e₁ ⇓ ⦇ ℰ' , λ x . e ⦈      ℰ₁ ⊢ e₂ ⇓ v₂      ℰ₂ = ℰ'[x ↦ v₂]      ℰ₂ ⊢ e ⇓ v
       ───────────────────────────────────────────────────────────────────────────────────── (appE)
                                          ℰ ⊢ e₁ e₂ ⇓ v
    *)
    ignore (e1, e2); assert false (* TODO *)
  | Let (x, e1, e2) ->
    (*
       ℰ ⊢ e₁ ⇓ v₁      ℰ' = ℰ[x ↦ v₁]      ℰ' ⊢ e₂ ⇓ v
       ─────────────────────────────────────────────────── (letE)
                    ℰ ⊢ let x = e₁ in e₂ ⇓ v
    *)
    ignore (x, e1, e2); assert false (* TODO *)

let interp (s : string) : value option =
  match parse s with
  | Some expr ->
    begin
      match eval Env.empty expr with
      | v -> Some v
      | exception _ -> print_endline "Eval error"; None
    end
  | None -> print_endline "Parser error"; None
