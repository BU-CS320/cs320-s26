%{
    open Ast
    open Utils
%}

%token LET "let"
%token REC "rec"
%token TYPE "type"
%token OF "of"
%token IN "in"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token FUN "fun"
%token MATCH "match"
%token WITH "with"
%token MOD "mod"
%token ASSERT "assert"
%token UNIT "unit"
%token BOOL "bool"
%token INT "int"
%token STRING "string"
%token TRUE "true"
%token FALSE "false"

%token ARROW "->"
%token AND "&&"
%token OR "||"
%token CONS "::"
%token LTE "<="
%token GTE ">="
%token NEQ "<>"
%token ALT "|"
%token COLON ":"
%token SEMI ";"
%token COMMA ","
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token EQ "="
%token LT "<"
%token GT ">"
%token CARAT "^"
%token LPAREN "("
%token RPAREN ")"
%token LBRAK "["
%token RBRAK "]"
%token WILDCARD "_"

%token<string> VAR_ID
%token<string> TPAR_ID
%token<string> CONSTR_ID

%token<int> INT_LIT
%token<string> STRING_LIT

%token EOF

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left CARAT
%right CONS
%left PLUS MINUS
%left STAR SLASH MOD

%start<Ast.prog> prog

%%

prog:
  | stmts = stmt* EOF { stmts }

delim(d, x):
  | d x=x { x }

arg:
  | x=VAR_ID { (x, None) }
  | "(" x=VAR_ID ty=delim(":", ty) ")" { (x, Some ty) }

constr_decl:
  | "|" name=CONSTR_ID { name, None }
  | "|" name=CONSTR_ID "of" t=ty { name, Some t }

stmt:
  | "let" x=VAR_ID args=arg* ty=delim(":", ty)? "=" e=expr
    { Stmt.let_ $loc false x args ty e }
  | "let" "rec" x=VAR_ID args=arg+ ty=delim(":", ty)? "=" e=expr
    { Stmt.let_ $loc true x args ty e }
  | "type" tpars=tpars? name=VAR_ID "=" constrs=constr_decl+
    { Stmt.adt $loc (Option.value ~default:[] tpars) name constrs }

tpars:
  | p=TPAR_ID { [p] }
  | "(" p=TPAR_ID ps=delim(",", TPAR_ID)+ ")" { p :: ps }

ty:
  | t1=ty "->" t2=ty { Type.fun_ $loc t1 t2 }
  | t=ty2 { t }

ty2:
  | t=ty3 ts=delim("*", ty3)+ { Type.tuple $loc (t :: ts) }
  | t=ty3 { t }

ty3:
  | "unit" { Type.unit $loc }
  | "bool" { Type.bool $loc }
  | "int" { Type.int $loc }
  | "string" { Type.string $loc }
  | name=VAR_ID { Type.adt $loc [] name }
  | t=ty3 name=VAR_ID { Type.adt $loc [t] name }
  | "(" t=ty ts=delim(",", ty)+ ")" name=VAR_ID { Type.adt $loc (t :: ts) name }
  | a=TPAR_ID { Type.param $loc a }
  | "(" ty=ty ")" { ty }

expr_(last):
  | "let" name=VAR_ID args=arg* annot=delim(":", ty)? "=" e1=expr "in" e2=last
    { Expr.let_ $loc false name args annot e1 e2 }
  | "let" "rec" name=VAR_ID args=arg+ annot=delim(":", ty)? "=" e1=expr "in" e2=last
    { Expr.let_ $loc true name args annot e1 e2 }
  | "if" e1=expr "then" e2=expr "else" e3=last
    { Expr.if_ $loc e1 e2 e3 }
  | "fun" xs=arg+ "->" e=last
    { Expr.fun_ $loc xs e }

expr:
  | e=expr_(expr) { e }
  | e=expr2_(expr3) { e }

expr_no_match:
  | e=expr_(expr_no_match) { e }
  | e=expr3 { e }

expr2_(last):
  | "match" e=expr "with" cs=case+ { Expr.match_ $loc e cs }
  | e=last { e }

pattern:
  | p1=pattern2 p2=delim(",", pattern2)+ { Pattern.tuple $loc (p1 :: p2) }
  | p=pattern2 { p }

pattern2:
  | p1=pattern2 "::" p2=pattern2 { Pattern.cons $loc "Cons" (Some (Pattern.tuple dummy_pos [p1; p2])) }
  | p=pattern3 { p }

pattern3:
  | "_" { Pattern.wild $loc }
  | "true" { Pattern.true_ $loc }
  | "false" { Pattern.false_ $loc }
  | n=INT_LIT { Pattern.int $loc n }
  | s=STRING_LIT { Pattern.string $loc s }
  | l=list_lit(pattern) { Pattern.list_lit $loc l }
  | x=VAR_ID { Pattern.var $loc x }
  | c=CONSTR_ID arg=pattern3? { Pattern.cons $loc c arg }
  | "(" p=pattern? ")" { Option.value ~default:(Pattern.unit $loc) p }

nonempty_semi_list(x):
  | x=x { [x] }
  | x=x ";" { [x] }
  | x=x ";" xs=nonempty_semi_list(x) { x :: xs }

semi_list(x):
  | es=nonempty_semi_list(x)? { Option.value es ~default:[] }

list_lit(x):
  | "[" xs=semi_list(x) "]" { xs }

case:
  | "|" p=pattern "->" e=expr_no_match { (p, e) }

%inline bop:
  | "+" { Expr.Add }
  | "-" { Expr.Sub }
  | "*" { Expr.Mul }
  | "/" { Expr.Div }
  | "mod" { Expr.Mod }
  | "=" { Expr.Eq }
  | "<>" { Expr.Neq }
  | "<" { Expr.Lt }
  | "<=" { Expr.Lte }
  | ">" { Expr.Gt }
  | ">=" { Expr.Gte }
  | "&&" { Expr.And }
  | "||" { Expr.Or }
  | "^" { Expr.Concat }

expr3:
  | e=expr4 es=delim(",", expr4)+ { Expr.tuple $loc (e :: es) }
  | e=expr4 { e }

expr4:
  | e1=expr4 "::" e2=expr4 { Expr.cons $loc "Cons" (Some (Expr.tuple dummy_pos [e1; e2])) }
  | e1=expr4 op=bop e2=expr4 { Expr.bop $loc e1 op e2 }
  | "assert" e=expr5 { Expr.assert_ $loc e }
  | "-" e=expr5 { Expr.negate $loc e }
  | c=CONSTR_ID arg=expr5? { Expr.cons $loc c arg }
  | e=expr5 es=expr5+
    {
      let folder acc (e : Expr.t) =
        let (l_pos, _) = $loc in
        let (_, r_pos) = e.pos in
        Expr.app (l_pos, r_pos) acc e
      in
      List.fold_left folder e es
    }
  | e=expr5 { e }

expr5:
  | "(" ")" { Expr.unit $loc }
  | "true" { Expr.true_ $loc }
  | "false" { Expr.false_ $loc }
  | n=INT_LIT { Expr.int $loc n }
  | s=STRING_LIT { Expr.string $loc s }
  | l=list_lit(expr) { Expr.list_lit $loc l }
  | x=VAR_ID { Expr.var $loc x }
  | "(" e=expr ty=delim(":", ty)? ")"
    {
      match ty with
      | None -> e
      | Some ty -> Expr.annot $loc e ty
    }
