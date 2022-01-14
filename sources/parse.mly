%{
	open Ast
%}

// Tokens expressions
%token	<string>		ID
%token	<int>			CSTE
%token	<string>		STRING
%token	<string>		CLASSNAME
%token	<Ast.opComp>	RELOP

// Fake token for unit operators priority
%token	UNIT

// Operator tokens for integer expressions
%token	PLUS MINUS TIMES DIV

// '&' token for string concatenation
%token	CONCAT

// '(' and ')' tokens
%token	LPAREN RPAREN

// '{' and '}' tokens
%token	LBRACE RBRACE

// ':' and ';' tokens
%token	COLON SEMICOLON

// '\'' and '"' tokens
%token QUOUTE DOUBLEQUOTE

// Classes tokens
%token	CLASS
%token	DEF
%token	STATIC
%token	OVERRIDE
%token	EXTENDS
%token	IS
%token	VAR
%token	COMMA
%token	DOT
%token	NEW

// Instruction tokens
%token	RETURN
%token	ASSIGN
%token	IF THEN ELSE

// End of file token
%token	EOF

// Priorities
%left		CONCAT
%nonassoc	RELOP
%left		PLUS MINUS
%left		TIMES DIV
%left		UNIT
%left		DOT

// Main rule
%start	<Ast.prog>	program
%%

program:

	// Program
	| ld = list(declaration)
		li = delimited(LBRACE, list(instruction), RBRACE) EOF					{ prog(ld, li) }

declaration:

	// Class declaration
	| CLASS className = CLASSNAME
		lparam = delimited(LPAREN, separated_list(COMMA,
		constructorParameters), RPAREN) superClassOpt =
		option(EXTENDS superClass = CLASSNAME { superClass }) IS
		ce = delimited(LBRACE, list(classElement), RBRACE)						{ decl(className, lparam, superClassOpt, ce) }

constructorParameters:

	// Constructor parameters
	| o = boption(VAR) param = separated_nonempty_list(COMMA, ID)
		COLON className = CLASSNAME												{ constrParam(o, param, className) }

methodParameters:

	// Method parameters
	| param = separated_nonempty_list(COMMA, ID) COLON className = CLASSNAME	{ methodParam(param, className) }

classElement:

	// Fields
	| VAR s = boption(STATIC) name = ID COLON
		className = CLASSNAME SEMICOLON											{ Field(s, name, className) }

	// Constructors
	| DEF className = CLASSNAME
		lparam = delimited(LPAREN, list(constructorParameters), RPAREN)
		superClassOpt = option(COLON superClass = CLASSNAME { superClass })
		IS b = block															{ Constr(className, lparam, superClassOpt, b) }

	// Simple Methods
	| DEF s = boption(STATIC) o = boption(OVERRIDE) name = ID
		lparam = delimited(LPAREN, separated_list(COMMA,
		constructorParameters), RPAREN) COLON
		className = CLASSNAME ASSIGN e = expression								{ SimpleMethod(s, o, name, lparam, className, e) }

	// Complex Methods
	| DEF s = boption(STATIC) o = boption(OVERRIDE) name = ID
		lparam = delimited(LPAREN, separated_list(COMMA,
		constructorParameters), RPAREN) className = CLASSNAME IS b = block		{ ComplexMethod(s, o, name, lparam, className, b) }

block:

	// Instructions block
	| l = delimited(LBRACE, list(instruction), RBRACE)							{ Block(l) }

	// Local variables in Instructions block
	| LBRACE var = separated_nonempty_list(COMMA, methodParameters) IS
		li = nonempty_list(instruction) RBRACE									{ BlockVar(var, li) }

expression:

	// Identifier
	| x = ID																	{ Id x }

	// Integer constant
	| v = CSTE																	{ Cste v }

	// String
	| s = delimited(DOUBLEQUOTE ,STRING, DOUBLEQUOTE)							{ String s }

	// Expression in parentheses
	| e = delimited(LPAREN, expression, RPAREN)									{ e }

	// Cast
	| LPAREN x = CLASSNAME e = expression RPAREN								{ Cast(x, e) }

	// Instantiation
	| NEW x = CLASSNAME lparam = delimited(LPAREN,
		separated_list(COMMA, expression), RPAREN)								{ Instantiation(x, lparam) }

	// Field access
	| e = expression DOT x = ID													{ FieldAccess(e, x) }

	// Method call
	| e = expression DOT x = ID lparam = delimited(LPAREN,
		separated_list(COMMA, expression), RPAREN)								{ MethodCall(e, x, lparam) }

	// Operator expressions
	| l = expression op = RELOP r = expression									{ Comp(op, l, r) }
	| l = expression PLUS r = expression										{ Plus(l, r) }
	| l = expression MINUS r = expression										{ Minus(l, r) }
	| l = expression TIMES r = expression										{ Times(l, r) }
	| l = expression DIV r = expression											{ Div(l, r) }
	| l = expression CONCAT r = expression										{ Concat(l, r) }
	| PLUS e = expression %prec UNIT											{ e }
	| MINUS e = expression %prec UNIT											{ UMinus e }

instruction:

	// Expression with a semicolon
	| e = expression SEMICOLON													{ Expr e }

	// Block
	| b = block																	{ b }

	// Return whit a semicolon
	| RETURN SEMICOLON															{ Return }

	// Assignment
	| l = expression ASSIGN r = expression SEMICOLON							{ Assignment(l, r) }

	// If then else
	| IF i = expression THEN t = instruction ELSE e = instruction				{ Ite(i, t, e) }
