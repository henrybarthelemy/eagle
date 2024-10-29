%{ open Typ
   open Expr %}

%token <string> IDENT
%token LOCALITY

%token EOF

%start <Expr.expr option> expr_opt

%%

let expr_opt :=
  | EOF; { None }
  | e = expr; EOF; { Some e }

let terminal ==
  | LOCALITY; i = IDENT; { DeclerationExpr (LocalityDecl i) }
  | i = IDENT; { Variable i }

let expr :=
  | terminal
