%{
  open Ast
  let debug msg = () (* print_endline msg *)
%}

%token <string> IDENT
%token NAME
%token COMMA
%token NONCE
%token LPAREN
%token RPAREN
%token INPUT
%token EQUALS
%token DEF
%token NEW_LINE
%token EOF

%start <Ast.program> program

%%

let program :=
  | instructions = instructions; { 
      debug "Parsed: program";
      Program(instructions) 
  } 

let instructions := 
  | instruction = instruction; new_lines; roi = instructions; { instruction:: roi }
  | instruction = instruction; EOF; { [instruction] }
  | EOF; { [] }

let new_lines := 
  | NEW_LINE; { debug "Parsed: new line" }
  | NEW_LINE; new_lines; { debug "Parsed: multiple new lines" }

let instruction :=
  | declaration = declaration; { 
      debug "Parsed: instruction declaration";
      Declaration(declaration) 
    }  
  | DEF; i = IDENT; LPAREN; RPAREN; EQUALS; body = body; { 
      debug ("Parsed: function definition with identifier " ^ i);
      FunctionDef(i, [], body)
    }

let declaration :=
  | NAME; i = IDENT; { 
      debug ("Parsed: declaration with name " ^ i);
      Name(i) 
    }

let body :=
  | new_lines; expr = expr; rob = body; { 
      debug "Parsed: body with expression";
      expr :: rob 
    }
  | EOF; { 
      debug "Parsed: empty body";
      [] 
    }     

let expr := 
  | INPUT; i = IDENT; { 
      debug ("Parsed: input expression with identifier " ^ i);
      Input(i) 
    }            
