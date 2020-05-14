%{
  open Ast;;

  let rec mkfun params expr = match params with
  | [] -> expr
  | p :: prms -> EFun(p, mkfun prms expr)
  ;;
%}

%token <int> INT 
%token <string> IDENT
%token <float> FLOAT
%token PLUS MINUS TIMES DIV EGAL
%token LPAREN RPAREN SEMICOLON COMMA DOT
%token LET IN ADD REMOVE ARROW FUN
%token SPHERE PLANE SOURCE EYE
%left EGAL
%left PLUS MINUS
%left TIMES DIV

%start main
%type <Ast.expr> main

%%

main: expr SEMICOLON { $1 }
    | SEMICOLON main { $2 }
;

expr :
 | LET IDENT largs EGAL expr IN expr            { ELet ($2, (mkfun $3 $5), $7) }
 | FUN IDENT ARROW expr                         { EFun ($2, $4) }
 | ADD expr                                     { EAdd ($2) }
 | REMOVE expr                                  { ERemove ($2) }
 | obj                                          { EObject $1 }
 | arithm_expr                                  { $1 }

base_expr:
 INT                     { EInt $1 }
 | IDENT                 { EIdent $1 }
 | LPAREN	expr RPAREN    { $2 }
 | FLOAT                 { EFloat $1 }


obj :
 | SPHERE LPAREN triplet COMMA FLOAT COMMA color_triplet COMMA FLOAT RPAREN
     { Sphere ({s_origin = $3;s_radius = $5;s_color = $7; s_refl =$9 }) }
 | PLANE LPAREN triplet COMMA triplet COMMA color_triplet COMMA FLOAT RPAREN
     { Plane ({p_origin = $3;p_normal = $5;p_color = $7; p_refl =$9 }) }
 | SOURCE LPAREN triplet COMMA triplet COMMA color_triplet RPAREN
     { Source ({l_origin = $3 ; l_direction = $5;l_color = $7 }) }
 | EYE LPAREN triplet COMMA triplet RPAREN
     { Eye ({e_origin = $3;e_direction = $5}) }


triplet:
  | LPAREN FLOAT COMMA FLOAT COMMA FLOAT RPAREN { { x = $2 ; y = $4; z = $6 } }

color_triplet:
  | LPAREN INT COMMA INT COMMA INT RPAREN { { r = $2 ; g = $4; b = $6 } }

arithm_expr :
 | arithm_expr PLUS arithm_expr               { EBinop ("+", $1, $3) }
 | arithm_expr MINUS arithm_expr              { EBinop ("-", $1, $3) }
 | arithm_expr TIMES arithm_expr              { EBinop ("*", $1, $3) }
 | arithm_expr DIV arithm_expr                { EBinop ("/", $1, $3) }
 | arithm_expr EGAL arithm_expr               { EBinop ("=", $1, $3) }
 | application                                { $1 }

application :
 | application base_expr  { EApp ($1, $2)}
 | MINUS base_expr        { EMonop ("-", $2) }
 | base_expr              { $1 }

largs:
 | IDENT largs { $1::$2 }
 | { [] }

;
