open Sedlexing.Utf8
open Parser
open Ast

exception Invalid_token
exception Invalid_name_type

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]

let colon = [%sedlex.regexp? ':']

let comma = [%sedlex.regexp? ',']

let name = [%sedlex.regexp? "name"]

let name_type = [%sedlex.regexp? "nonce"]

let at = [%sedlex.regexp? '@']

let new_line = [%sedlex.regexp? '\n']

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']

let number = [%sedlex.regexp? '0' .. '9']

let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]

let int = [%sedlex.regexp? Plus number]

let locality = [%sedlex.regexp? "locality"]


let match_to_name_type x =
  match x with 
  | "nonce" -> Nonce
  | _ -> 
    let error_message = "Unable to find type " ^ x in
    print_endline error_message;
    raise Invalid_name_type 

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | new_line -> NEW_LINE
  | locality -> LOCALITY
  | colon -> COLON
  | comma -> COMMA
  | at -> AT
  | name -> NAME
  | name_type -> let name_typ = match_to_name_type (lexeme buf) in
  NAME_TYPE name_typ
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