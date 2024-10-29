%{
  open Ast 
%}

%token <string> IDENT

%token NAME
%token COLON
%token AT
%token COMMA
%token NONCE
%token LOCALITY
%token NEW_LINE
%token EOF


%start <Ast.program> program

%%

let program :=
  | instruction = instruction; new_lines; roi = program; EOF; { Instruction(instruction, roi) } 
  | instruction = instruction; EOF; { Instruction(instruction, EmptyProgram) } 
  | EOF; { EmptyProgram }

let new_lines := 
  | NEW_LINE
  | NEW_LINE; new_lines

let instruction :=
  | declaration = declaration; { Declaration(declaration) }  
  | expr = expr; { Expression(expr) }          

let declaration :=
  | LOCALITY; i = IDENT; { LocalityDecl(Locality(i)) }  
  | NAME; i = IDENT; COLON; NONCE; AT; lis = localities; { NameDecl(Name(i, Nonce, lis))}

let localities := 
  | li = IDENT; { [Locality(li)] }
  | li = IDENT; COMMA; lis = localities; { Locality(li) :: lis }

let expr :=
  | terminal = terminal; { terminal }                    

let terminal :=
  | i = IDENT; { Variable(i) }              
