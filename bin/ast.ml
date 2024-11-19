
type declaration =
  | Name of string
[@@deriving show { with_path = false }]

type aterm = 
  | Get of string
  | Function of string * aterm list
  | RetVariable of string
[@@deriving show { with_path = false }]

type l_typ = 
  | EnumType of string * string list option (* Type identifier and possible params *)
[@@deriving show { with_path = false }]

type expr = 
  | Ret of aterm
  | Output of expr * expr option
  | Input of string * expr
  | Enc of aterm * aterm
  | Dec of aterm * aterm
  | Let of string * expr * expr
  | Match of aterm * (l_typ * expr) list (* type match with aterm and associated expr *)
[@@deriving show { with_path = false }]

type instruction = 
| Expression of expr
| Declaration of declaration
| FunctionDef of string * string list * expr

[@@deriving show { with_path = false }]

type program =
| Program of instruction list
[@@deriving show { with_path = false }]