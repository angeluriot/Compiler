%{
	open Ast
	open Print
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token SEMICOLON
%token ASSIGN
%token BEGIN END
%token IF THEN ELSE
%token EOF

%right ELSE
%left PLUS MINUS
%left TIMES DIV

%type <Ast.decl list * Ast.expType> prog
%type <Ast.expType> cond
%type <Ast.expType> expr
%type <Ast.decl> decl

%start prog
%%
prog: ld = list(decl) BEGIN e = expr END EOF	{ (ld, e) }

cond: g = expr o = RELOP d = expr				{ Comp(g, o, d) }
	| LPAREN c = cond RPAREN					{ c }

expr: i = ID									{ Id i }
	| c = CSTE									{ Cste c }
	| PLUS e = expr								{ e }
	| MINUS e = expr							{ UMinus(e) }
	| l = expr PLUS r = expr					{ Plus(l, r) }
	| l = expr MINUS r = expr					{ Minus(l, r) }
	| l = expr TIMES r = expr					{ Times(l, r) }
	| l = expr DIV r = expr						{ Div(l, r) }
	| LPAREN e = expr RPAREN					{ e }
	| IF c = cond THEN t = expr ELSE e = expr	{ Ite(c, t, e) }

decl: i = ID ASSIGN e = expr SEMICOLON			{ { lhs = i; rhs = e } }
