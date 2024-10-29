open Declarations

type expr =
  | Variable of string
  | DeclerationExpr of declaration
[@@deriving show { with_path = false }]