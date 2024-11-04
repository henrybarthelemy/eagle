open Parser
open Lexer
open Ast

let parse_expr = Lexer.from_string Parser.program

let print_expr code =
  parse_expr code |> Format.printf "%a\n%!" Ast.pp_program

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let code = read_whole_file "/Users/henrybarthelemy/eagle/bin/owlcode/example1.eagle"

let () = print_expr code