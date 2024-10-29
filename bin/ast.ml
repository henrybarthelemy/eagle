type nametyp = Nonce
[@@deriving show { with_path = false }]

type locality = Locality of string
[@@deriving show { with_path = false }]

type name = Name of string * nametyp * locality list
[@@deriving show { with_path = false }]

type declaration = 
| LocalityDecl of locality
| NameDecl of name
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
| Instruction of instruction * program
[@@deriving show { with_path = false }]