module Context = Map.Make (String)

open Parser
open Lexer
open Expr


let parse_expr = Lexer.from_string Parser.expr_opt

let print_expr code =
  parse_expr code |> Option.get |> Format.printf "%a\n%!" Expr.pp_expr

let () = print_expr "locality alice"