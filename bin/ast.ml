type enum = string * string list
[@@deriving show { with_path = false }]

type declaration =
  | Name of string
  | Enum of enum
[@@deriving show { with_path = false }]

type aterm = 
  | Get of string
  | Function of string * aterm list
  | RetVariable of string
[@@deriving show { with_path = false }]

type expr = 
  | Ret of aterm
  | Output of expr
  | Input of string * (expr list)
  | Enc of aterm * aterm
  | Dec of aterm * aterm
  | Let of string * expr * (expr list) 
  | Match of aterm * (expr * expr) list
[@@deriving show { with_path = false }]

type instruction = 
| Expression of expr
| Declaration of declaration
| FunctionDef of string * string list * expr list

[@@deriving show { with_path = false }]

type program =
| Program of instruction list
[@@deriving show { with_path = false }]