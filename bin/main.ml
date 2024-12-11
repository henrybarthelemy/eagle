open Parser
open Lexer
open Translator

(* Function to parse a program string *)
let parse_program = Lexer.from_string Parser.program

(* Convert to file *)
let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


(* Console Printing *)
let print_program filename =
  let code = read_whole_file filename in
  let parsed_program = parse_program code in
  Format.printf "%a\n%!" pretty_program parsed_program

(* Convert to file *)
let program_to_string filename =
  let code = read_whole_file filename in
  let parsed_program = parse_program code in
  (* Use Format.str_formatter to build a string *)
  Format.fprintf Format.str_formatter "%a@!" pretty_program parsed_program;
  Format.flush_str_formatter ()

(* Write to file util takes string string -> void *)
let write_to_file filename content =
  let oc = open_out filename in
  try
    output_string oc content;
    flush oc;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

(* WIP Needham-Schroeder *)
(* let () =
  let file_in_name = "/Users/henrybarthelemy/eagle/bin/owlcode/needham_schroeder.eagle" in
  let file_out_name = "/Users/henrybarthelemy/eagle/bin/sapic/NeedhamSchroeder.spthy" in
  print_program file_in_name;
  write_to_file file_out_name (program_to_string file_in_name); *)

let () =
  let fin = "/Users/henrybarthelemy/eagle/bin/owlcode/example1.eagle" in
  let fon = "/Users/henrybarthelemy/eagle/bin/sapic/Example1.spthy" in
  print_program fin;
  write_to_file fon (program_to_string fin); 