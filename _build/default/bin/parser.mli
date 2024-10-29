
(* The type of tokens. *)

type token = 
  | LOCALITY
  | IDENT of (string)
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val expr_opt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr option)
