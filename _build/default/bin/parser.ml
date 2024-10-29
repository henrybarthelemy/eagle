
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | LOCALITY
    | IDENT of (
# 4 "bin/parser.mly"
       (string)
# 16 "bin/parser.ml"
  )
    | EOF
  
end

include MenhirBasics

# 1 "bin/parser.mly"
   open Typ
   open Expr 
# 27 "bin/parser.ml"

type ('s, 'r) _menhir_state

and _menhir_box_expr_opt = 
  | MenhirBox_expr_opt of (Expr.expr option) [@@unboxed]

let _menhir_action_1 =
  fun _1 i ->
    (
# 18 "bin/parser.mly"
                         ( DeclerationExpr (LocalityDecl i) )
# 39 "bin/parser.ml"
     : (Expr.expr))

let _menhir_action_2 =
  fun i ->
    (
# 19 "bin/parser.mly"
               ( Variable i )
# 47 "bin/parser.ml"
     : (Expr.expr))

let _menhir_action_3 =
  fun _1 ->
    (
# 14 "bin/parser.mly"
         ( None )
# 55 "bin/parser.ml"
     : (Expr.expr option))

let _menhir_action_4 =
  fun _2 e ->
    (
# 15 "bin/parser.mly"
                   ( Some e )
# 63 "bin/parser.ml"
     : (Expr.expr option))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | EOF ->
        "EOF"
    | IDENT _ ->
        "IDENT"
    | LOCALITY ->
        "LOCALITY"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_expr_opt : type  ttv_stack. ttv_stack -> _ -> _menhir_box_expr_opt =
    fun _menhir_stack _v ->
      MenhirBox_expr_opt _v
  
  let _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_opt =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let (_2, e) = ((), _v) in
          let _v = _menhir_action_4 _2 e in
          _menhir_goto_expr_opt _menhir_stack _v
      | _ ->
          _eRR ()
  
  let _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_opt =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LOCALITY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (i, _1) = (_v, ()) in
              let _v = _menhir_action_1 _1 i in
              _menhir_goto_expr _menhir_stack _v _tok
          | _ ->
              _eRR ())
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_2 i in
          _menhir_goto_expr _menhir_stack _v _tok
      | EOF ->
          let _1 = () in
          let _v = _menhir_action_3 _1 in
          _menhir_goto_expr_opt _menhir_stack _v
  
end

let expr_opt =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_expr_opt v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
