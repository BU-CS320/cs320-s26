{
    open Utils
    open Parser

    type error =
      | Unclosed_comment
      | Unknown_token of string
      | Unterminated_string
      | Illegal_string_char of string

    exception Error of pos * error

    let position b = Lexing.(lexeme_start_p b, Lexing.lexeme_end_p b)
}

let int_lit = ['0'-'9']+
let var_id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let tpar_id = '\'' ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let constr_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let whitespace = [' ' '\t' '\r']+

rule read =
  parse
  | "let" { LET }
  | "rec" { REC }
  | "type" { TYPE }
  | "of" { OF }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "match" { MATCH }
  | "with" { WITH }
  | "mod" { MOD }
  | "assert" { ASSERT }
  | "unit" { UNIT }
  | "bool" { BOOL }
  | "int" { INT }
  | "string" { STRING }
  | "true" { TRUE }
  | "false" { FALSE }
  | "->" { ARROW }
  | "&&" { AND }
  | "||" { OR }
  | "::" { CONS }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "(*" { read_comment 0 lexbuf }
  | "|" { ALT }
  | ":" { COLON }
  | ";" { SEMI }
  | "," { COMMA }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | "^" { CARAT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRAK }
  | "]" { RBRAK }
  | '\"' { read_string (Buffer.create 17) lexbuf }
  | int_lit { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
  | var_id { VAR_ID (Lexing.lexeme lexbuf) }
  | tpar_id { TPAR_ID (Lexing.lexeme lexbuf) }
  | constr_id { CONSTR_ID (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | _ { raise (Error (position lexbuf, Unknown_token (Lexing.lexeme lexbuf))) }
  | eof { EOF }

and read_comment depth =
  parse
  | "(*" { read_comment (depth + 1) lexbuf }
  | "*)"
    {
      if depth = 0
      then read lexbuf
      else read_comment (depth - 1) lexbuf
    }
  | '\n' { Lexing.new_line lexbuf; read_comment depth lexbuf }
  | [^ '(' '*']+ { read_comment depth lexbuf }
  | ['(' '*'] { read_comment depth lexbuf }
  | _ { raise (Error (position lexbuf, Unclosed_comment)) }

and read_string buf =
  parse
  | '"'       { STRING_LIT (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error (position lexbuf, Illegal_string_char (Lexing.lexeme lexbuf))) }
  | eof { raise (Error (position lexbuf, Unterminated_string)) }
