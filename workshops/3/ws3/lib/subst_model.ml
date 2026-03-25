
type expr = Ast.expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type value =
  | VNum of int
  | VFun of string * expr

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e

let expr_of_value v =
  match v with
  | VNum n -> Num n
  | VFun (x, e) -> Fun (x, e)

let rec subst v x e =
  match e with
  | Var y ->
    (* [v / x] y = y     (when y ≠ x)
       [v / x] x = x
    *)
    if x = y then expr_of_value v else e
  | Num n ->
    (* [v / x] n = n *)
    Num n
  | Fun (y, e') ->
    (* [v / x] (fun y → e) = fun y → [v / x] e    (when y ≠ x)
       [v / x] (fun x → e) = fun x → e
    *)
    Fun (y, if x = y then e' else subst v x e')
  | App (e1, e2) ->
    (* [v / x](e₁ e₂) = ([v / x]e₁)([v / x]e₂) *)
    App (subst v x e1, subst v x e2)
  | Let (y, e1, e2) ->
    (* [v / x](let y = e₁ in e₂) = let y = [v / x]e₁ in [v / x]e₂    (when y ≠ x)
       [v / x](let x = e₁ in e₂) = let x = [v / x]e₁ in e₂
    *)
    Let (y, subst v x e1, if x = y then e2 else subst v x e2)

let rec eval (e : expr) : value =
  match e with
  | Var _ ->
    (* CANNOT EVALUATE VARIABLE IN THE SUBSTITUTION MODEL *)
    assert false
  | Num n ->
    (*
       n is an int lit
       ─────────────── (intLitE)
           n ⇓ n
    *)
    VNum n
  | Fun (x, e) ->
    (*
       ───────────────────── (funE)
       fun x -> e ⇓ λ x . e
    *)
    VFun (x, e)
  | App (e1, e2) ->
    (*
       e₁ ⇓ λ x . e      e₂ ⇓ v₂      e' = [v₂ / x]e      e' ⇓ v
       ─────────────────────────────────────────────────────────── (appE)
                             ℰ ⊢ e₁ e₂ ⇓ v
    *)
    begin
      match eval e1 with
      | VFun (x, e) -> eval (subst (eval e2)  x e)
      | _ -> assert false
    end
  | Let (x, e1, e2) ->
    (*
       e₁ ⇓ v₁    e = [v₁ / x]e₂      e ⇓ v
       ───────────────────────────────────── (letE)
             ℰ ⊢ let x = e₁ in e₂ ⇓ v
    *)
    let v1 = eval e1 in
    let e = subst v1 x e2 in
    let v = eval e in
    v

let rec fv e =
  match e with
  | Var x -> [x]
  | Num _ -> []
  | Fun (x, e) -> List.filter ((<>) x) (fv e)
  | App (e1, e2) -> fv e1 @ fv e2
  | Let (x, e1, e2) -> fv e1 @ List.filter ((<>) x) (fv e2)

let closed e = fv e = []

let interp (s : string) : value option =
  match parse s with
  | Some expr ->
    if closed expr
    then
      match eval expr with
      | v -> Some v
      | exception _ -> print_endline "Eval error"; None
    else None
  | None -> print_endline "Parse error"; None
