open Ast

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

(* Hard coded for now; TODO: Add input in the syntax to take this information in *)
let pretty_program fmt (Program instrs) =
  Format.fprintf fmt "theory ExampleOwl\nbegin\n\n"; 
  Format.fprintf fmt "builtins: symmetric-encryption\n\nprocess:\n!(";
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n\t|| ")
    pretty_instruction fmt instrs;
  Format.fprintf fmt ")\n\nend"
