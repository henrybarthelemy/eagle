open Sedlexing.Utf8
open Parser

exception Invalid_token
exception Invalid_name_type

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]
let comma = [%sedlex.regexp? ',']
let name = [%sedlex.regexp? "name"]
let nonce = [%sedlex.regexp? "nonce"]
let new_line = [%sedlex.regexp? '\n']

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']
let upper_alpha = [%sedlex.regexp? 'A' .. 'Z']
let number = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]
let typeident = [%sedlex.regexp? upper_alpha, Star (lower_alpha | upper_alpha | number | '_')]
let int = [%sedlex.regexp? Plus number]

let lparen =  [%sedlex.regexp? '(']
let rparen =  [%sedlex.regexp? ')']
let equals =  [%sedlex.regexp? '=']
let def = [%sedlex.regexp? "def"]
let input = [%sedlex.regexp? "input"]
let in_regex = [%sedlex.regexp? "in"]
let ret = [%sedlex.regexp? "ret"]
let get = [%sedlex.regexp? "get"]
let output = [%sedlex.regexp? "output"]
let dec = [%sedlex.regexp? "dec"]
let enc = [%sedlex.regexp? "enc"]
let let_regex = [%sedlex.regexp? "let"]
let end_regex = [%sedlex.regexp? "end"]
let secret_name = [%sedlex.regexp? "secretname"]

let match_reg = [%sedlex.regexp? "match"]
let implies = [%sedlex.regexp? "=>"]
let mid_bar = [%sedlex.regexp? "|"]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | end_regex -> END
  | new_line -> NEW_LINE
  | def -> DEF
  | match_reg -> MATCH
  | implies -> IMPLIES
  | mid_bar -> MID_BAR
  | lparen -> LPAREN
  | rparen -> RPAREN
  | equals -> EQUALS
  | input -> INPUT
  | let_regex -> LET
  | comma -> COMMA
  | secret_name -> SECRET_NAME
  | name -> NAME
  | in_regex -> IN
  | ret -> RET
  | get -> GET
  | output -> OUTPUT
  | dec -> DEC
  | enc -> ENC
  | nonce -> NONCE
  | typeident -> TYIDENT (lexeme buf)
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