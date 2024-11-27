open Parser
open Lexer
open Ast

(* Function to parse a program string *)
let parse_program = Lexer.from_string Parser.program

(* Pretty printer for the AST *)

let rec pretty_aterm fmt aterm =
  match aterm with
  | Get name -> Format.fprintf fmt "%s" name
  | RetVariable name -> Format.fprintf fmt "%s" name
  | Function (name, args) ->
      Format.fprintf fmt "%s(%a)" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pretty_aterm)
        args

let rec pretty_expr fmt expr =
  match expr with
  | Ret aterm -> Format.fprintf fmt "Ret(%a)" pretty_aterm aterm
  | Output (e, None) -> Format.fprintf fmt "out(%a)" pretty_expr e
  | Output (e, Some e2) -> Format.fprintf fmt "out(%a); %a" pretty_expr e pretty_expr e2
  | Input (name, e) -> Format.fprintf fmt "in(%s, %a)" name pretty_expr e
  | Enc (e1, e2) -> Format.fprintf fmt "senc(%a, %a)" pretty_aterm e1 pretty_aterm e2
  | Dec (e1, e2) -> Format.fprintf fmt "sdec(%a, %a)" pretty_aterm e1 pretty_aterm e2
  | Let (name, e1, e2) ->
      Format.fprintf fmt "let %s = %a in %a" name pretty_expr e1 pretty_expr e2
  | Match (_, _) -> () (* Ignored for now *)

let pretty_declaration fmt decl =
  match decl with
  | Name name -> Format.fprintf fmt "new %s;" name

let rec pretty_instruction fmt instr =
  match instr with
  | Declaration decl -> pretty_declaration fmt decl
  | Expression expr -> pretty_expr fmt expr
  | FunctionDef (name, params, body) ->
      Format.fprintf fmt "(%s(%a) { %a })" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string)
        params pretty_expr body

let pretty_program fmt (Program instrs) =
  Format.fprintf fmt "theory ExampleOwl\nbegin\n\n"; (* Need to add to be able to take this in on parser level*)
  Format.fprintf fmt "builtins: symmetric-encryption\n\nprocess:\n!(";
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\t|| ")
    pretty_instruction fmt instrs;
  Format.fprintf fmt ")\n\nend"


let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let program_to_string filename =
  let code = read_whole_file filename in
  let parsed_program = parse_program code in
  (* Use Format.str_formatter to build a string *)
  Format.fprintf Format.str_formatter "%a@!" pretty_program parsed_program;
  Format.flush_str_formatter ()

let print_program filename =
  let code = read_whole_file filename in
  let parsed_program = parse_program code in
  Format.printf "%a\n%!" pretty_program parsed_program

let write_to_file filename content =
  let oc = open_out filename in
  try
    output_string oc content;
    flush oc;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let () =
  let file_in_name = "/Users/henrybarthelemy/eagle/bin/owlcode/example1.eagle" in
  let file_out_name = "/Users/henrybarthelemy/eagle/bin/sapic/Example1out.spthy" in
  print_program file_in_name;
  write_to_file file_out_name (program_to_string file_in_name);
  



