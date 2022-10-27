{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1 }
}

let digits = ['0' - '9']+
let ws = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let symbol = [ ^ ' ' '\t' '\n' '\r' '(' ')' '&' ]+
let boolean = "true" | "false"

rule read = parse
| "def" { DEF }
| "end" { END }
| "if" { IF }
| "else" { ELSE }
| "then" { THEN }
| "()" { NIL }
| "(" { LPAREN }
| ")" { RPAREN }
| "&" { AMPERSAND }
| ws { read lexbuf }
| newline { next_line lexbuf; read lexbuf }

| boolean { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
| digits { INT (int_of_string (Lexing.lexeme lexbuf)) }

| eof { EOF }
| symbol { SYMBOL (Lexing.lexeme lexbuf) }