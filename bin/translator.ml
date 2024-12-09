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
  | Ret aterm -> Format.fprintf fmt "%a" pretty_aterm aterm
  | Output (e, None) -> Format.fprintf fmt "out(%a)" pretty_expr e
  | Output (e, Some e2) -> Format.fprintf fmt "out(%a); \n %a" pretty_expr e pretty_expr e2
  | Input (name, e) -> Format.fprintf fmt "in(%s); \n %a" name pretty_expr e
  | Enc (e1, e2) -> Format.fprintf fmt "senc(%a, %a)" pretty_aterm e1 pretty_aterm e2
  | Dec (e1, e2) -> Format.fprintf fmt "sdec(%a, %a)" pretty_aterm e1 pretty_aterm e2
  | Let (name, e1, e2) ->
      Format.fprintf fmt "let %s = %a in %a \n" name pretty_expr e1 pretty_expr e2
  | Match (_, _) -> () (* Ignored for now *)

let pretty_declaration fmt decl =
  match decl with
  | Name name -> Format.fprintf fmt "new %s;" name

let rec pretty_instruction fmt instr =
  match instr with
  | Declaration decl -> Format.fprintf fmt "%a " pretty_declaration decl
  | Expression expr -> Format.fprintf fmt "|| \n (%a) \n" pretty_expr expr
  | FunctionDef (name, params, body) ->
      Format.fprintf fmt "|| \n (%a) \n" pretty_expr body


(* Hard coded for now; TODO: Add input in the syntax to take this information in *)
let pretty_program fmt (Program instrs) =
  Format.fprintf fmt "theory ExampleOwl\nbegin\n\n"; 
  Format.fprintf fmt "builtins: symmetric-encryption\n\nprocess:\n!(";
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
    pretty_instruction fmt instrs;
  Format.fprintf fmt ")\n\nend"



(**** *)

let rec pretty_instruction fmt instr in_function_group =
  match instr with
  | Declaration decl -> 
      if in_function_group then Format.fprintf fmt "))\n"; (* Close group if needed *)
      Format.fprintf fmt "%a " pretty_declaration decl;
      false (* Not in a function group anymore *)
  | Expression expr -> 
      if in_function_group then Format.fprintf fmt "))\n"; (* Close group if needed *)
      Format.fprintf fmt "|| \n (%a) \n" pretty_expr expr;
      false (* Not in a function group anymore *)
  | FunctionDef (name, params, body) ->
      if not in_function_group then
        Format.fprintf fmt "((\n" (* Start group if not already in one *)
      else
        Format.fprintf fmt "||\n"; (* Add separator between functions in a group *)
      Format.fprintf fmt "(%a)\n" pretty_expr body;
      true (* Remain in a function group *)

let pretty_program fmt (Program instrs) =
  Format.fprintf fmt "theory ExampleOwl\nbegin\n\n"; 
  Format.fprintf fmt "builtins: symmetric-encryption\nfunctions: pred/1\n\nprocess:\n!(";

  (* Use a fold to track function grouping state *)
  let in_function_group =
    List.fold_left
      (fun in_function_group instr ->
         pretty_instruction fmt instr in_function_group)
      false (* Initial state: not in a function group *)
      instrs
  in

  (* Close any remaining open function group at the end *)
  if in_function_group then Format.fprintf fmt "))\n";

  Format.fprintf fmt ")\n\nend"


