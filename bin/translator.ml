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

let rec pretty_instruction fmt instr prev_instr =
  match instr with
  | Declaration decl -> Format.fprintf fmt "%a " pretty_declaration decl
  | Expression expr -> Format.fprintf fmt "|| \n (%a) \n" pretty_expr expr
  | FunctionDef (name, params, body) ->
      (match prev_instr with
       | Some (FunctionDef _) ->
           (* Special formatting when a FunctionDef follows another FunctionDef *)
           Format.fprintf fmt "|| (%a) \n" pretty_expr body
       | _ ->
           Format.fprintf fmt "(%a) \n" pretty_expr body)

let pretty_program fmt (Program instrs) =
  Format.fprintf fmt "theory ExampleOwl\nbegin\n\n"; 
  Format.fprintf fmt "builtins: symmetric-encryption\n\nprocess:\n!(";

  (* Use a fold to keep track of the previous instruction while iterating *)
  let _ =
    List.fold_left
      (fun prev_instr instr ->
         pretty_instruction fmt instr prev_instr;
         Some instr) (* Update the previous instruction *)
      None (* Initial previous instruction *)
      instrs
  in

  Format.fprintf fmt ")\n\nend"
