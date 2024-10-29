module Context = Map.Make (String)

open Parser
open Lexer
open Ast


let parse_expr = Lexer.from_string Parser.program

let print_expr code =
  parse_expr code |> Format.printf "%a\n%!" Ast.pp_program


let () = print_expr "locality alice \n locality bob" 


(*
let print_expr code =
  parse_expr code |> Option.get |> Format.printf "%a\n%!" Expr.pp_expr

let () = print_expr "locality alice" *)