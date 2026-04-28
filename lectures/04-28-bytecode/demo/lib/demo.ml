open Utils

let parse s = Parser.prog Lexer.read (Lexing.from_string s)

type bop = Utils.bop

module Env = Map.Make(String)

type value =
  | VBool of bool
  | VNum of int
  | VClos of string option * string * expr * value Env.t

let string_of_value = function
  | VBool b -> string_of_bool b
  | VNum n -> string_of_int n
  | _ -> "<fun>"

let rec eval env =
  let num v = match v with VNum n -> n | _ -> assert false in
  let bool v = match v with VBool b -> b | _ -> assert false in
  let rec go = function
    | Bool b -> VBool b
    | Num n -> VNum n
    | Var x -> Env.find x env
    | Bop (op, e1, e2) ->
      begin
        match op with
        | Add -> VNum (num (go e1) + num (go e2))
        | Sub -> VNum (num (go e1) - num (go e2))
        | Mul -> VNum (num (go e1) * num (go e2))
        | Div -> VNum (num (go e1) / num (go e2))
        | Lt -> VBool (num (go e1) < num (go e2))
        | Lte -> VBool (num (go e1) <= num (go e2))
        | Gt -> VBool (num (go e1) > num (go e2))
        | Gte -> VBool (num (go e1) >= num (go e2))
        | Eq -> VBool (num (go e1) = num (go e2))
        | Neq -> VBool (num (go e1) <> num (go e2))
        | And -> if bool (go e1) then go e2 else VBool false
        | Or -> if bool (go e1) then VBool true else go e2
      end
    | If (e1, e2, e3) -> if bool (go e1) then go e2 else go e3
    | Let (x, e1, e2) -> eval (Env.add x (go e1) env) e2
    | LetRec (f, Fun (y, e1), e2) ->
      eval (Env.add f (VClos (Some f, y, e1, env)) env) e2
    | LetRec _ -> assert false
    | Fun (x, e) -> VClos (None, x, e, env)
    | App (e1, e2) ->
      begin
        match go e1 with
        | VClos (None, x, e, env) -> eval (Env.add x (go e2) env) e
        | VClos (Some f, x, e, env) ->
          let env =
            env
            |> Env.add f (VClos (Some f, x, e, env))
            |> Env.add x (go e2)
          in eval env e
        | _ -> assert false
      end
  in go

type expr =
  | Bool of bool
  | Num of int
  | Var of int
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Fun of expr
  | FunRec of expr

let expr_of_expr =
  let rec go vars =
    let rec go' : Utils.expr -> expr = function
      | Bool b -> Bool b
      | Num n -> Num n
      | Var x -> Var (Option.get (List.find_index ((=) x) vars))
      | Bop (op, e1, e2) -> Bop (op, go' e1, go' e2)
      | If (e1, e2, e3) -> If (go' e1, go' e2, go' e3)
      | App (e1, e2) -> App (go' e1, go' e2)
      | Fun (x, e) -> Fun (go (x :: vars) e)
      | Let (x, e1, e2) -> App (Fun (go (x :: vars) e2), go' e1)
      | LetRec (f, Fun (x, e1), e2) -> App (Fun (go (f :: vars) e2), FunRec (go (f :: x :: vars) e1))
      | LetRec _ -> assert false
    in go'
  in go []

let emit = output_byte stdout
let eint = output_binary_int stdout
let ebop op =
  let code_of_bop = function
    | Add -> 1
    | Sub -> 2
    | Mul -> 3
    | Div -> 4
    | Lt -> 5
    | Lte -> 6
    | Gt -> 7
    | Gte -> 8
    | Eq -> 9
    | Neq -> 10
    | And -> 11
    | Or -> 12
  in
  emit (code_of_bop op)
let epush () = emit 0
let eif () = emit 13
let efun () = emit 14
let efunr () = emit 15
let ecall () = emit 16
let eret () = emit 17
let elook () = emit 18

let int_offset = 4

let offset =
  let rec go = function
    | Bool _ | Num _ | Var _ -> 1 + int_offset
    | App (e1, e2) | Bop (_, e1, e2) -> go e1 + go e2 + 1
    | If (e1, e2, e3) ->
      go e1                                         (* e1 *)
      + 1 + int_offset                              (* IF offset *)
      + go e2                                       (* e2 *)
      + 1 + int_offset + 1 + int_offset             (* PUSH 0 IF OFFSET *)
      + go e3                                       (* e3 *)
    | Fun e | FunRec e -> 1 + int_offset + go e + 1 (* ? *)
  in go

let compile e =
  let rec go = function
  | Bool b -> epush (); eint (if b then 1 else 0)
  | Num n -> epush (); eint n
  | Var n -> elook (); eint n
  | App (e1, e2) -> go e2; go e1; ecall ()
  | Bop (op, e1, e2) -> go e2; go e1; ebop op
  | If (e1, e2, e3) ->
    begin
      go e1;
      eif ();
      eint (offset e2 + 1 + int_offset + 1 + int_offset);
      go e2;
      epush (); eint 0;
      eif ();
      eint (offset e3);
      go e3;
    end
  | Fun e ->
    begin
      efun ();
      eint (offset e + 1);
      go e;
      eret ();
    end
  | FunRec e ->
    begin
      efunr ();
      eint (offset e + 1);
      go e;
      eret ();
    end
  in e |> expr_of_expr |> go
