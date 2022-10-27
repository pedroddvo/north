%{
    open Ast
%}

%token <int> INT
%token <bool> BOOLEAN
%token <string> SYMBOL

%token EOF

%token NIL
%token DEF END
%token IF THEN ELSE

%token LPAREN RPAREN
%token AMPERSAND



%start program

%type <Ast.expr list> program

%%

program:
    | es = list(expr); EOF { es }
    | EOF { [] }
    ;

func_def:
    DEF
    name = SYMBOL
    LPAREN
    args = terminated(list(expr), RPAREN)
    exprs = terminated(list(expr), END)
    { Def (name, args, exprs) }
    ;

if_expr:
    IF
    cond = terminated(list(expr), THEN)
    t = terminated(list(expr), ELSE)
    f = terminated(list(expr), END)
    { If (cond, t, f) }
    ;

grouping: 
    LPAREN
    group = terminated(list(expr), RPAREN)
    { Grouping group }
    ;

anon_func:
    AMPERSAND; LPAREN
    group = terminated(list(in_anon_func), RPAREN)
    { AnonFunc group }
    ;

in_anon_func:
    | AMPERSAND; x = INT { AnonId x }
    | e = expr { e }
    ;

expr:
    | AMPERSAND; x = anon_func { ExprRef x }
    | AMPERSAND; x = SYMBOL { ExprRef ( Symbol x ) }
    | x = anon_func { x }
    | x = func_def { x }
    | x = if_expr { x }
    | x = grouping { x }
    | x = INT { Int x }
    | x = SYMBOL { Symbol x }
    | x = BOOLEAN { Boolean x }
    | NIL { Nil }
    ;