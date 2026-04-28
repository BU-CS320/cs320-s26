%{
open Utils
%}

%token IF "if"
%token THEN "then"
%token ELSE "else"
%token LET "let"
%token REC "rec"
%token EQ "="
%token IN "in"
%token FUN "fun"
%token ARR "->"
%token TRUE "true"
%token FALSE "false"
%token LPAREN "("
%token RPAREN ")"
%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"
%token AND "&&"
%token OR "||"
%token<int> NUM
%token<string> VAR
%token EOF

%right OR
%right AND
%left LT, LTE, GT, GTE, EQ, NEQ
%left ADD, SUB
%left MUL, DIV

%start <Utils.prog> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr { If (e1, e2, e3) }
  | "let" v = VAR "=" e1 = expr "in" e2 = expr { Let (v, e1, e2) }
  | "let" "rec" x = VAR "=" e1 = expr "in" e2 = expr { LetRec (x, e1, e2) }
  | "fun" v = VAR "->" e = expr { Fun (v, e) }
  | e = expr2 { e }

%inline bop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }
  | "<" { Lt }
  | "<=" { Lte }
  | ">"  { Gt }
  | ">=" { Gte }
  | "="  { Eq }
  | "<>" { Neq }
  | "&&" { And }
  | "||" { Or }

expr2:
  | e1 = expr2 op = bop e2 = expr2 { Bop (op, e1, e2) }
  | es = expr3+ { List.(fold_left (fun apps e -> App (apps, e)) (hd es) (tl es)) }

expr3:
  | "true" { Bool true}
  | "false" { Bool false }
  | n = NUM { Num n }
  | v = VAR { Var v }
  | "(" e = expr ")" { e }
