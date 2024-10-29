type declaration = 
| LocalityDecl of string
[@@deriving show { with_path = false }]

type expr =
  | Variable of string
[@@deriving show { with_path = false }]

type instruction = 
| Expression of expr
| Declaration of declaration
[@@deriving show { with_path = false }]

type program =
| EmptyProgram
| Instructions of instruction * program
[@@deriving show { with_path = false }]