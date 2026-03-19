%{
    open Ast
%}

/* keywords */
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
%token LIST "list"
%token TRUE "true"
%token FALSE "false"

/* symbols */
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

/* identifiers */
%token<string> VAR_ID
%token<string> TPAR_ID
%token<string> CONSTR_ID

/* literals */
%token<int> INT_LIT
%token<string> STRING_LIT

%token EOF

/* associativity and precedence */
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

delim(d, x):
  | d x=x { x }

prog:
  | stmts = stmt* EOF { stmts }

stmt:
  | "let" rec_="rec"? x=VAR_ID args=arg* ty=delim(":", ty)? "=" e=expr
    { Stmt.let_ $loc (Option.is_some rec_) x args ty e }
  | "type" tpars=tpars? name=VAR_ID "=" constrs=constr_decl*
    { Stmt.adt $loc (Option.value ~default:[] tpars) name constrs }

constr_decl:
  | "|" name=CONSTR_ID { name, [] }
  | "|" name=CONSTR_ID "of" t=ty ts=delim("*", ty)* { name, t :: ts }

tpars:
  | a=TPAR_ID { [a] }
  | "(" p=TPAR_ID ps=delim(",", TPAR_ID)+ ")" { p :: ps }

ty:
  | name=VAR_ID { Type.adt $loc [] name }
  | t=ty name=VAR_ID { Type.adt $loc [t] name }
  | t=ty "list" { Type.list $loc t }
  | "(" t=ty ts=delim(",", ty)+ ")" name=VAR_ID
    { Type.adt $loc (t :: ts) name }
  | ty=ty2 { ty }
ty2:
  | t1=ty2 "->" t2=ty2 { Type.fun_ $loc t1 t2 }
  | "unit" { Type.unit $loc }
  | "bool" { Type.bool $loc }
  | "int" { Type.int $loc }
  | "string" { Type.string $loc }
  | a=TPAR_ID { Type.param $loc a }
  | "(" ty=ty ")" { ty }

arg:
  | x=VAR_ID { (x, None) }
  | "(" x=VAR_ID ty=delim(":", ty) ")" { (x, Some ty) }

expr_(last):
  | "let" rec_="rec"? name=VAR_ID args=arg* annot=delim(":", ty)? "=" e1=expr "in" e2=last
    { Expr.let_ $loc (Option.is_some rec_) name args annot e1 e2 }
  | "if" e1=expr "then" e2=expr "else" e3=last
    { Expr.if_ $loc e1 e2 e3 }
  | "fun" args=arg+ "->" body=last
    { Expr.fun_ $loc args body }

expr:
  | e=expr_(expr) { e }
  | e=expr2 { e }

expr_no_match:
  | e=expr_(expr_no_match) { e }
  | e=expr3 { e }

expr2:
  | "match" e=expr es=delim(",", expr)* "with" cs=case+ { Expr.match_ $loc (e :: es) cs }
  | e=expr3 { e }

pattern:
  | p=pattern "::" ps=pattern { Pattern.cons $loc p ps }
  | p=pattern2 { p }

pattern2:
  | "(" ")" { Pattern.unit $loc }
  | "true" { Pattern.true_ $loc }
  | "false" { Pattern.false_ $loc }
  | n=INT_LIT { Pattern.int $loc n }
  | s=STRING_LIT { Pattern.string $loc s }
  | l=list_lit(pattern) { Pattern.list_lit $loc l }
  | x=VAR_ID { Pattern.var $loc x }
  | c=CONSTR_ID { Pattern.constr $loc c [] }
  | c=CONSTR_ID arg=pattern2 { Pattern.constr $loc c [arg] }
  | c=CONSTR_ID "(" p=pattern ps=delim(",", pattern)+ ")"
    { Pattern.constr $loc c (p :: ps) }
  | "(" p=pattern ")" { p }

list_lit(x):
  | "[" "]" { [] }
  | "[" x=x xs=delim(";", x)* "]" { x :: xs }

case:
  | "|" p=pattern ps=delim(",", pattern)* "->" e=expr_no_match { (p :: ps, e) }

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
  | "::" { Expr.Cons }
  | "^" { Expr.Concat }

expr3:
  | e1=expr3 op=bop e2=expr3 { Expr.bop $loc e1 op e2 }
  | "assert" e=expr4 { Expr.assert_ $loc e }
  | "-" e=expr4 { Expr.negate $loc e }
  | c=CONSTR_ID { Expr.constr $loc c [] }
  | c=CONSTR_ID arg=expr4 { Expr.constr $loc c [arg] }
  | c=CONSTR_ID "(" p=expr ps=delim(",", expr)+ ")"
    { Expr.constr $loc c (p :: ps) }
  | es=expr4+ { Expr.app $loc (List.hd es) (List.tl es) }

expr4:
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
