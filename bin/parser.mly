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
%token IN
%token RET
%token GET
%token OUTPUT
%token DEC
%token ENC
%token LET


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
  | INPUT; i = IDENT; IN; body = body; { 
      debug ("Parsed: input expression with identifier " ^ i);
      Input(i, body) 
    }
  | RET; LPAREN; aterm = aterm; RPAREN; {
    debug ("Parsed: retreieve expression");
    Ret(aterm)
    }      
  | OUTPUT; expr = expr; { 
    debug ("Parsed: output expression");
    Output(expr) 
    }
  | ENC; LPAREN; aterm1 = aterm; COMMA; aterm2 = aterm; RPAREN; {
    debug ("Parsed: encryption with two aterms");
    Enc(aterm1, aterm2)
    }
  | DEC; LPAREN; aterm1 = aterm; COMMA; aterm2 = aterm; RPAREN; {
    debug ("Parsed: encryption with two aterms");
    Dec(aterm1, aterm2)
    }
  | LET; i = IDENT; EQUALS; eq_expr = expr; IN; body = body; {
      debug ("Parsed: let with identity " ^ i);
      Let(i, eq_expr, body)
  }

let aterm := 
  | GET; LPAREN; n = IDENT; RPAREN; { 
    debug ("Parsed a get with name " ^ n);
    Get(n) 
    }
  | i = IDENT; { 
    debug ("Parsed a retrieve with param " ^ i);
    RetVariable(i) 
    }     