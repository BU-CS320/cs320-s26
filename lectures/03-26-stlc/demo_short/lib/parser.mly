%{
open Ast
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN
%token RPAREN
%token LET
%token EQ
%token IN
%token <string> VAR
%token <int> NUM

%token COLON
%token INT

%right ARROW

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }

ty:
  | INT { Int }
  | t1=ty; ARROW; t2=ty { TFun (t1, t2) }
  | LPAREN; ty=ty; RPAREN { ty }

expr:
  | LET; x=VAR; EQ; e1=expr; IN; e2=expr { Let(x, e1, e2) }
  | FUN; LPAREN; x=VAR; COLON; ty=ty RPAREN; ARROW; e=expr { Fun(x, ty, e) }
  | e = expr2 { e }

expr2:
  | e = expr3; es = expr3*
    { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

expr3:
  | x = VAR { Var x }
  | n = NUM { Num n }
  | LPAREN; e=expr; RPAREN { e }
