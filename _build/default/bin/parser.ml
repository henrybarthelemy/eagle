
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | NEW_LINE
    | LOCALITY
    | IDENT of (
# 5 "bin/parser.mly"
       (string)
# 17 "bin/parser.ml"
  )
    | EOF
  
end

include MenhirBasics

# 1 "bin/parser.mly"
  
  open Ast 

# 29 "bin/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_program) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState08 : (('s, _menhir_box_program) _menhir_cell1_instruction, _menhir_box_program) _menhir_state
    (** State 08.
        Stack shape : instruction.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_instruction = 
  | MenhirCell1_instruction of 's * ('s, 'r) _menhir_state * (Ast.instruction)

and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_01 =
  fun _1 i ->
    (
# 26 "bin/parser.mly"
                         ( LocalityDecl(i) )
# 54 "bin/parser.ml"
     : (Ast.declaration))

let _menhir_action_02 =
  fun terminal ->
    (
# 29 "bin/parser.mly"
                         ( terminal )
# 62 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun declaration ->
    (
# 22 "bin/parser.mly"
                               ( Declaration(declaration) )
# 70 "bin/parser.ml"
     : (Ast.instruction))

let _menhir_action_04 =
  fun expr ->
    (
# 23 "bin/parser.mly"
                 ( Expression(expr) )
# 78 "bin/parser.ml"
     : (Ast.instruction))

let _menhir_action_05 =
  fun _2 _4 instruction roi ->
    (
# 15 "bin/parser.mly"
                                                             ( Instructions(instruction, roi) )
# 86 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_06 =
  fun _2 instruction roi ->
    (
# 16 "bin/parser.mly"
                                                        ( Instructions(instruction, roi) )
# 94 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_07 =
  fun _2 _3 instruction ->
    (
# 17 "bin/parser.mly"
                                              ( Instructions(instruction, EmptyProgram) )
# 102 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_08 =
  fun _2 instruction ->
    (
# 18 "bin/parser.mly"
                                    ( Instructions(instruction, EmptyProgram) )
# 110 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_09 =
  fun _1 ->
    (
# 19 "bin/parser.mly"
         ( EmptyProgram )
# 118 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_10 =
  fun i ->
    (
# 32 "bin/parser.mly"
               ( Variable(i) )
# 126 "bin/parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | EOF ->
        "EOF"
    | IDENT _ ->
        "IDENT"
    | LOCALITY ->
        "LOCALITY"
    | NEW_LINE ->
        "NEW_LINE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      MenhirBox_program _v
  
  let rec _menhir_goto_program : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState08 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_06 _menhir_stack _v
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_instruction -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_instruction (_menhir_stack, _menhir_s, instruction) = _menhir_stack in
          let (_4, roi, _2) = ((), _v, ()) in
          let _v = _menhir_action_05 _2 _4 instruction roi in
          _menhir_goto_program _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (i, _1) = (_v, ()) in
          let _v = _menhir_action_01 _1 i in
          let declaration = _v in
          let _v = _menhir_action_03 declaration in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_instruction : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NEW_LINE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LOCALITY ->
              let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
          | IDENT _v_0 ->
              let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState08
          | EOF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_2, instruction, _3) = ((), _v, ()) in
              let _v = _menhir_action_07 _2 _3 instruction in
              _menhir_goto_program _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | EOF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_2, instruction) = ((), _v) in
          let _v = _menhir_action_08 _2 instruction in
          _menhir_goto_program _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_10 i in
      let terminal = _v in
      let _v = _menhir_action_02 terminal in
      let expr = _v in
      let _v = _menhir_action_04 expr in
      _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LOCALITY ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | IDENT _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00
      | EOF ->
          let _v =
            let _1 = () in
            _menhir_action_09 _1
          in
          _menhir_run_06 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
