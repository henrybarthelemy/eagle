type declaration = 
  | LocalityDecl of string
[@@deriving show { with_path = false }]