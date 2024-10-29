%{
  open Ast 
%}

%token <string> IDENT
%token LOCALITY
%token NEW_LINE
%token EOF

%start <Ast.program> program

%%

let program :=
  | instruction = instruction; NEW_LINE; roi = program; EOF; { Instructions(instruction, roi) } 
  | instruction = instruction; NEW_LINE; roi = program; { Instructions(instruction, roi) } 
  | instruction = instruction; NEW_LINE; EOF; { Instructions(instruction, EmptyProgram) } 
  | instruction = instruction; EOF; { Instructions(instruction, EmptyProgram) } 
  | EOF; { EmptyProgram }

let instruction :=
  | declaration = declaration; { Declaration(declaration) }  
  | expr = expr; { Expression(expr) }          

let declaration :=
  | LOCALITY; i = IDENT; { LocalityDecl(i) }  

let expr :=
  | terminal = terminal; { terminal }                    

let terminal :=
  | i = IDENT; { Variable(i) }              
