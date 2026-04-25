include Utils

(* SYNTAX *)

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

(* TYPE INFERENCE *)

type constr = ty * ty
type solution = (string * ty) list
(* [
     x₁ ↦ τ₁;
     x₂ ↦ τ₂;
     x₃ ↦ τ₃;
   ]
*)

let is_free a t =
  let rec go = function
    | TInt | TBool -> false
    | TFun (t1, t2) -> go t1 || go t2
    | TPar b -> a = b
  in go t

let subst t1 a t2 =
  let rec go = function
    | TInt -> TInt
    | TBool -> TBool
    | TFun (in_t, out_t) -> TFun (go in_t, go out_t)
    | TPar b -> if a = b then t1 else TPar b
  in go t2

let unify (cs : constr list) : solution option =
  let rec go acc cs =
    match cs with
    | [] -> Some (List.rev acc)
    | (t1, t2) :: cs ->
      begin
        match t1, t2 with
        | _ when t1 = t2 -> go acc cs
        | TFun (in1, out1), TFun (in2, out2) -> go acc ((in1, in2) :: (out1, out2) :: cs)
        | TPar a, t when not (is_free a t) ->
          go ((a, t) :: acc) (List.map (fun (t1, t2) -> subst t a t1, subst t a t2) cs)
        | t, TPar a -> go acc ((TPar a, t) :: cs)
        | _ -> None
      end
  in go [] cs

(* Fresh type variable generator *)
let counter = ref 0
let fresh () =
  let _ = counter := !counter + 1 in
  TPar ("$" ^ string_of_int !counter)


type ctxt = ty_scheme Env.t

(* helper function for adding monotypes to the context *)
let add x ty ctxt = Env.add x ([], ty) ctxt

(* Γ ⊢ e : τ ⊣ 𝒞 *)
let rec constrs_of (ctxt : ctxt) (e : expr) : ty * constr list =
  let rec go e =
    match e with
    | Num _ -> TInt, []
    | Add (e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      TInt, (t1, TInt) :: (t2, TInt) :: cs1 @ cs2
    | Eq (e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      TBool , (t1, t2) :: cs1 @ cs2
    | If (e1, e2, e3) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = go e2 in
      let t3, cs3 = go e3 in
      t3, (t2, t3) :: (t1, TBool) :: cs1 @ cs2 @ cs3
    | Let (x, e1, e2) ->
      let t1, cs1 = go e1 in
      let t2, cs2 = constrs_of (add x t1 ctxt) e2 in
      t2, cs1 @ cs2
    | Fun (x, e) ->
      let a = fresh () in
      let t, cs = constrs_of (add x a ctxt) e in
      TFun (a, t), cs
    | App (e1, e2) ->
      (* Γ ⊢ e₁ : τ₁ ⊣ 𝒞₁ *)
      let t1, cs1 = go e1 in
      (* Γ ⊢ e₂ : τ₂ ⊣ 𝒞₂ *)
      let t2, cs2 = go e2 in
      (* α is fresh *)
      let a = fresh () in
      (* Γ ⊢ e₁ e₂ : α ⊣ τ₁ ≐ τ₂ → α, 𝒞₁, 𝒞₂ *)
      a, (t1, TFun (t2, a)) :: cs1 @ cs2
    | Var x ->
      match Env.find_opt x ctxt with
      | None -> TInt, [TInt, TBool]
      | Some (var, ty) ->
        let ty =
          List.fold_left
            (fun ty a -> subst (fresh ()) a ty)
            ty
            var
        in
        ty, []
  in go e

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: nub (List.filter ((<>) x) xs)

let free_vars (ty : ty) : string list =
  let rec go ty =
    match ty with
    | TInt -> []
    | TBool -> []
    | TFun (t1, t2) -> go t1 @ go t2
    | TPar a -> [a]
  in nub (go ty)

let principal (sol : solution) (ty : ty) : ty_scheme =
  (* apply sol to ty *)
  let ty =
    List.fold_left
      (fun t1 (a, t2) -> subst t2 a t1)
      ty
      sol
  in
  (* find free variables of ty *)
  let fv = free_vars ty in
  (* quantify over free variables *)
  fv, ty

let is_well_typed (p : prog) : bool =
  let rec go ctxt p =
    match p with
    | [] -> true
    (* let x = e ... *)
    | (x, e) :: p ->
      (* 1. Constraint-based inference *)
      let ty, cs = constrs_of ctxt e in
      (* 2. Unification *)
      let sol = unify cs in
      (* 3. Generalization (principal type) *)
      match sol with
      | None -> false
      | Some sol ->
        let pty = principal sol ty in
        let _ = print_endline (string_of_ty_scheme pty) in
        go (Env.add x pty ctxt) p
  in go Env.empty p

(* SEMANTICS *)

let rec eval (env : dyn_env) (e : expr) : value =
  let rec go e =
    let go_int e = match go e with | VNum n -> n | _ -> assert false in
    let go_bool e = match go e with | VBool b -> b | _ -> assert false in
    match e with
    | Let(x, e1, e2) -> eval (Env.add x (go e1) env) e2
    | App (e1, e2) -> (
        match go e1 with
        | VClos (x, e, env') -> eval (Env.add x (go e2) env') e
        | _ -> assert false
      )
    | Var x -> Env.find x env
    | Num n -> VNum n
    | Fun (x, e) -> VClos (x, e, env)
    | Add (e1, e2) -> VNum (go_int e1 + go_int e2)
    | Eq (e1, e2) -> VBool (go_int e1 = go_int e2)
    | If (e1, e2, e3) -> if go_bool e1 then go e2 else go e3
  in go e

let rec nest_lets = function
  | [] -> assert false
  | (name, binding) :: [] -> Let (name, binding, Var name)
  | (name, binding) :: p ->
    Let (name, binding, nest_lets p)

let interp (s : string) : value option =
  match parse s with
  | Some p ->
    if is_well_typed p
    then Some (eval Env.empty (nest_lets p))
    else None
  | None -> None
