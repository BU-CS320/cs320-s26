
type command =
  | PUSH
  | ADD | SUB | MUL | DIV
  | LT | LTE | GT | GTE | EQ | NEQ
  | AND | OR
  | IF
  | FUN | FUNREC
  | CALL | RETURN
  | LOOKUP

let command_of_byte c =
  let rec go = function
    | 0 -> PUSH
    | 1 -> ADD
    | 2 -> SUB
    | 3 -> MUL
    | 4 -> DIV
    | 5 -> LT
    | 6 -> LTE
    | 7 -> GT
    | 8 -> GTE
    | 9 -> EQ
    | 10 -> NEQ
    | 11 -> AND
    | 12 -> OR
    | 13 -> IF
    | 14 -> FUN
    | 15 -> FUNREC
    | 16 -> CALL
    | 17 -> RETURN
    | 18 -> LOOKUP
    | _ -> assert false
  in go (int_of_char c)

type value =
  | Num of int32
  | Clos of int * value list
  | ClosRec of int * value list

let string_of_value = function
  | Num n -> Int32.to_string n
  | _ -> "<fun>"

let prog = Bytes.of_string In_channel.(input_all stdin)
let get_int32 bs = Bytes.get_int32_be prog bs
let get_int bs = Int32.to_int (get_int32 bs)

let rec eval (s, env, pc) =
  let rec go s env pc =
    match command_of_byte (Bytes.get prog pc), s with
    | PUSH, s -> Num (get_int32 (pc + 1)) :: s, env, pc + 5
    | ADD, Num m :: Num n :: s ->
      Num (Int32.add m n) :: s, env, pc + 1
    | SUB, Num m :: Num n :: s ->
      Num (Int32.sub m n) :: s, env, pc + 1
    | MUL, Num m :: Num n :: s ->
      Num (Int32.mul m n) :: s, env, pc + 1
    | DIV, Num m :: Num n :: s ->
      Num (Int32.div m n) :: s, env, pc + 1
    | LT, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n < 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | LTE, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n <= 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | GT, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n > 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | GTE, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n >= 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | EQ, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n = 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | NEQ, Num m :: Num n :: s ->
      ( Num (if Int32.compare m n <> 0 then 1l else 0l) :: s
      , env
      , pc + 1
      )
    | AND, Num m :: Num n :: s ->
      ( Num (if Int32.equal m 0l then 0l else n) :: s
      , env
      , pc + 1
      )
    | OR, Num m :: Num n :: s ->
      ( Num (if not (Int32.equal m 0l) then 1l else n) :: s
      , env
      , pc + 1
      )
    | IF, Num 0l :: s ->
      s, env, pc + 5 + get_int (pc + 1)
    | IF, Num _ :: s ->
      s, env, pc + 5
    | FUN, s ->
      Clos (pc + 5, env) :: s, env, pc + 5 + get_int (pc + 1)
    | FUNREC, s ->
      ClosRec (pc + 5, env) :: s, env, pc + 5 + get_int (pc + 1)
    | CALL, Clos (fun_pc, fun_env) :: v :: s ->
      Clos (pc + 1, env) :: s, v :: fun_env, fun_pc
    | CALL, ClosRec (fun_pc, fun_env) :: v :: s ->
      ( Clos (pc + 1, env) :: s
      , ClosRec (fun_pc, fun_env) :: v :: fun_env
      , fun_pc
      )
    | RETURN, v :: Clos (ret_pc, ret_env) :: s ->
      v :: s, ret_env, ret_pc
    | LOOKUP, s ->
      List.nth env (get_int (pc + 1)) :: s, env, pc + 5
    | _ -> assert false
  in
  if pc = Bytes.length prog
  then List.hd s
  else eval (go s env pc)

let () =
  ([], [], 0)
  |> eval
  |> string_of_value
  |> print_endline
