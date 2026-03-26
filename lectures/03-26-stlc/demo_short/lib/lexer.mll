{
open Parser
}

let whitespace = [' ' '\n' '\t' '\r']+
let var = ['a'-'z']+
let num = '-'? ['0'-'9']+

rule read =
  parse
  | "fun" { FUN }
  | "let" { LET }
  | "int" { INT }
  | "=" { EQ }
  | "in" { IN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }
