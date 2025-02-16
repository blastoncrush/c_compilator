(* Analyseur lexical *)

{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let id_or_kwd s = match s with
  | "int"      -> INT
  | "char"     -> CHAR
  | "return"   -> RETURN
  | "if"       -> IF
  | "else"     -> ELSE
  | "while"    -> WHILE
  | "break"    -> BREAK
  | "continue" -> CONTINUE
  | "malloc" -> MALLOC
  | "sizeof" -> SIZEOF
  | s -> IDENT s
}


let letter  = ['a'-'z' 'A'-'Z']
let digit   = ['0'-'9']
let ident   = letter (letter | digit | ['_'])*
let integer = digit+
let space   = [' ' '\t']
let char    = ['''] [' '-'~'] [''']
let str     = ['"'] [' '-'~']* ['"']

rule token = parse
  | '\n'    { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '&''&'  { AND }
  | '|''|'  { OR }
  | '!'     { NOT }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '='     { EQ }
  | '&'     { ADDR }
  | '('     { LP }
  | ')'     { RP }
  | '{'     { LB }
  | '}'     { RB }
  | '['     { LC }
  | ']'     { RC }
  | '=''='  { EQQ }
  | '!''='  { NEQ }
  | '>''='  { GEQ }
  | '<''='  { LEQ }
  | '>'     { GE }
  | '<'     { LE }
  | ';'     { SEMICOLON }
  | ','     { COMMA }
  | integer as s { CST_INT (int_of_string s) }
  | char as s { CST_CHAR (String.sub s 1 1) }
  | str as s { CST_STRING (String.sub s 1 (String.length s - 2)) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }
