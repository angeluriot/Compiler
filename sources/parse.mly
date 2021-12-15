%{
	open Ast
%}
%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN

%token IF THEN ELSE BEGIN END

/* utilise pour donner une precedence maximale au - unaire
 * L'analyseur lexical ne renvoie jamais ce token !
 */
%token UMINUS

%token EOF

%right ELSE
%left PLUS MINUS	/* lowest precedence */
%left TIMES DIV		/* medium precedence */
%left UMINUS		/* highest precedence */

%type <expType> expr bexpr
%type <decl> declaration

(* l'axiome sera aussi le nom de la fonction a appeler pour faire l'analyse
 * syntaxique
 *)
%start<Ast.progType> prog
%%
prog:  ld = list(declaration) BEGIN i = expr END EOF
  { ld, i }

declaration :
	x = ID ASSIGN e = expr SEMICOLON				{ { lhs = x; rhs = e; } }

expr:
	x = ID											{ Id x }
	| v = CSTE										{ Cste v }
	| g = expr PLUS d = expr						{ Plus (g, d) }
	| g = expr MINUS d = expr						{ Minus(g, d) }
	| g = expr TIMES d = expr						{ Times(g, d) }
	| g = expr DIV d = expr							{ Div(g, d) }
	| PLUS e = expr									{ e }
	| MINUS e = expr %prec UMINUS					{ UMinus e }
	| e = delimited (LPAREN, expr, RPAREN)			{ e }
	| IF si=bexpr THEN alors=expr ELSE sinon = expr	{ Ite(si, alors, sinon) }

bexpr :
	g = expr op = RELOP d = expr  { Comp(op, g, d) }
	| e = delimited (LPAREN, bexpr, RPAREN) { e }
