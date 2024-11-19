%{
  open Ast
  let debug msg = print_endline msg 
%}

%token <string> IDENT
%token <string> TYIDENT
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
%token MATCH
%token IMPLIES
%token MID_BAR
%token EOF


%start <Ast.program> program

%%

let program :=
  | instructions = instructions; { 
      debug "Parsed: program";
      Program(instructions) 
  } 

let instructions := 
  | instruction = instruction; newline1; roi = instructions; { instruction:: roi }
  | instruction = instruction; EOF; { [instruction] }
  | EOF; { [] }


let newline1 :=
  | NEW_LINE; { debug "Parsed: single newline1" }
  | NEW_LINE; newline1; { debug "Parsed: newline1" }

let newline0 :=
  | { debug "Parsed: newline0 base" }
  | NEW_LINE; newline0; { debug "Parsed: newline0" }

let instruction :=
  | declaration = declaration; { 
      debug "Parsed: instruction declaration";
      Declaration(declaration) 
    }  
  | DEF; i = IDENT; LPAREN; RPAREN; EQUALS; newline1; expr = expr; { 
      debug ("Parsed: function definition with identifier " ^ i);
      FunctionDef(i, [], expr) (* TODO: Add parameter support *)
    }

let declaration :=
  | NAME; i = IDENT; { 
      debug ("Parsed: declaration with name " ^ i);
      Name(i) 
    }

let terminal_expr := 
  | RET; LPAREN; aterm = aterm; RPAREN; {
    debug ("Parsed: retreieve expression");
    Ret(aterm)
    }     
  | ENC; LPAREN; aterm1 = aterm; COMMA; aterm2 = aterm; RPAREN; {
    debug ("Parsed: encryption with two aterms");
    Enc(aterm1, aterm2)
    }
  | DEC; LPAREN; aterm1 = aterm; COMMA; aterm2 = aterm; RPAREN; {
    debug ("Parsed: encryption with two aterms");
    Dec(aterm1, aterm2)
    }

let expr := 
  | INPUT; i = IDENT; IN; newline0; expr = expr; { 
      debug ("Parsed: input expression with identifier " ^ i);
      Input(i, expr) 
  }
  | terminal_expr
  | OUTPUT; expr = expr; { 
    debug ("Parsed: output expression");
    Output(expr, None) 
    }
  | OUTPUT; expr = expr; newline0; expr2 = expr; { 
    debug ("Parsed: output expression with continuation");
    Output(expr, Some expr2) 
    }
  | LET; i = IDENT; EQUALS; eq_expr = expr; IN; newline0; expr = expr; {
      debug ("Parsed: let with identity " ^ i);
      Let(i, eq_expr, expr)
  }
  | MATCH; a = aterm; newline0; cases = cases; { Match (a, cases)}

let cases := 
  | case = case; NEW_LINE; roc = cases; { case :: roc }
  | case = case; { [case] }

let case := 
  | MID_BAR; l_typ = l_typ; IMPLIES; expr = expr; { (l_typ, expr) }

let l_typ :=
  | lty = TYIDENT; { 
    debug ("Parsed l_typ with no params and identifier " ^ lty);
    EnumType(lty, None) 
    }
  | i = TYIDENT; eparms = parms; { 
    debug ("Parsed l_typ with params and identifier " ^ i);
    EnumType(i, Some eparms)
    }

let parms := 
  | i = IDENT; { 
    debug ("Parsed single param with " ^ i);
    [i] 
    }
  | i = IDENT; rop = parms; { 
    debug ("Parsed multiple params with current of " ^ i);
    i :: rop 
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