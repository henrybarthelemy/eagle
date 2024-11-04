open Sedlexing.Utf8
open Parser
open Ast

exception Invalid_token
exception Invalid_name_type

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]

let comma = [%sedlex.regexp? ',']

let name = [%sedlex.regexp? "name"]

let nonce = [%sedlex.regexp? "nonce"]

let new_line = [%sedlex.regexp? '\n']

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']

let number = [%sedlex.regexp? '0' .. '9']

let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]

let int = [%sedlex.regexp? Plus number]

let lparen =  [%sedlex.regexp? '(']
let rparen =  [%sedlex.regexp? ')']
let equals =  [%sedlex.regexp? '=']
let def = [%sedlex.regexp? "def"]

let input = [%sedlex.regexp? "input"]
let in_regex = [%sedlex.regexp? "in"]

let in_regex = [%sedlex.regexp? "in"]
let ret = [%sedlex.regexp? "ret"]
let get = [%sedlex.regexp? "get"]
let output = [%sedlex.regexp? "output"]
let dec = [%sedlex.regexp? "dec"]
let enc = [%sedlex.regexp? "enc"]
let let_regex = [%sedlex.regexp? "let"]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | new_line -> NEW_LINE
  | def -> DEF
  | lparen -> LPAREN
  | rparen -> RPAREN
  | equals -> EQUALS
  | input -> INPUT
  | let_regex -> LET
  | comma -> COMMA
  | name -> NAME
  | in_regex -> IN
  | ret -> RET
  | get -> GET
  | output -> OUTPUT
  | dec -> DEC
  | enc -> ENC
  | nonce -> NONCE
  | ident -> IDENT (lexeme buf)
  | eof -> EOF
  | _ -> 
    Printf.eprintf "Error: unexpected token";
    raise Invalid_token

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string f string =
  provider (from_string string)
  |> MenhirLib.Convert.Simplified.traditional2revised f