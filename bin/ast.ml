type enum = string * string list
[@@deriving show { with_path = false }]

type declaration =
  | Name of string
  | Enum of enum
[@@deriving show { with_path = false }]

type aterm = 
  | Get of string
  | Function of aterm list
[@@deriving show { with_path = false }]

type expr = 
  | Ret of aterm
  | Output of expr
  | Input of string
  | Enc of aterm * aterm
  | Dec of aterm * aterm
  | Let of string * expr * expr 
  | Match of aterm * (expr * expr) list
[@@deriving show { with_path = false }]

type instruction = 
| Expression of expr
| Declaration of declaration
| FunctionDef of string * string list * expr 

[@@deriving show { with_path = false }]

type program =
| EmptyProgram
| Instruction of instruction * program
[@@deriving show { with_path = false }]