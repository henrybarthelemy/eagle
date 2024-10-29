open Sedlexing.Utf8
open Parser

exception Invalid_token

let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]

let new_line = [%sedlex.regexp? '\n']

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']

let number = [%sedlex.regexp? '0' .. '9']

let ident = [%sedlex.regexp? lower_alpha, Star (lower_alpha | number | '_')]

let int = [%sedlex.regexp? Plus number]

let locality = [%sedlex.regexp? "locality"]
let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | new_line -> NEW_LINE
  | locality -> LOCALITY
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