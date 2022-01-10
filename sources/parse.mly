%{
    open Ast
%}

// tokens expressions
%token    <string> ID
%token    <int>    CSTE
%token    <string> STRING
%token    <string> CLASSNAME

%token    <Ast.opComp> RELOP

/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token    UMINUS

// tokens opérateurs binaires / unaires pour Integer
%token    PLUS MINUS TIMES DIV
// token opérateur binaire & (concaténation) pour String
%token    CONCAT

// tokens '(' et ')'
%token    LPAREN RPAREN
// tokens '{' et '}'
%token    LBRACE RBRACE
// tokens ':' et ';'
%token    COLON SEMICOLON

// tokens d'une définition de classe
%token    CLASS
%token    DEF
%token    STATIC
%token    OVERRIDE
%token    EXTENDS
%token    IS
%token    VAR

%token    COMMA
%token    DOT
%token    NEW

%token    THIS
%token    SUPER

// tokens instructions
%token    RETURN
%token    ASSIGN
%token    IF THEN ELSE

%token    EOF

%left     CONCAT
%nonassoc RELOP
%left     PLUS MINUS        /* lowest precedence */
%left     TIMES DIV         /* medium precedence */
%left     UMINUS            /* highest precedence */
%left     DOT

// %type <expType> expression
// %type <decl> declaration


(* l'axiome sera aussi le nom de la fonction a appeler pour faire l'analyse 
 * syntaxique
 *)
%start<Ast.progType> program
%%

program:
    | ld = list(declaration) li = delimited(LBRACE, list(instruction), RBRACE) EOF              { ld, li }

declaration:
    | CLASS classname = CLASSNAME 
      lparam = delimited(LPAREN, separated_list(COMMA, constructorParameter), RPAREN)
      superclasseOpt = option(EXTENDS superclass = CLASSNAME { superclass }) IS 
      LBRACE ce = list(classElement) RBRACE                                                     { decl(classname, lparam, superclasseOpt, ce) }

constructorParameter:
    | o = boption(VAR) param = separated_list(COMMA, ID) COLON classname = CLASSNAME            { constructorParameter(o, param, classname) }

methodParameter:
    | param = separated_list(COMMA, ID) COLON classname = CLASSNAME                             {param, classname }

classElement:
    // field
    | VAR s = boption(STATIC) nom = ID COLON classname = CLASSNAME SEMICOLON                    { }
    // method
    | DEF s = boption(STATIC) o = boption(OVERRIDE) nom = ID 
      lparam = delimited(LPAREN, separated_list(COMMA, constructorParameter), RPAREN) 
      COLON classname = ID ASSIGN e = expression                                                { }
    | DEF s = boption(STATIC) o = boption(OVERRIDE) nom = ID 
      lparam = delimited(LPAREN, separated_list(COMMA, constructorParameter), RPAREN)
      superclasseOpt = option(COLON superclass = CLASSNAME { superclass }) IS b = bloc          { }
    // constructor
    | DEF classname = CLASSNAME lparam = delimited(LPAREN, list(constructorParameter), RPAREN)
      superclassOpt = option(COLON superclass = CLASSNAME { superclass }) IS
      li = delimited(LBRACE, list(instruction) , RBRACE)                                        { constructor(classname, lparam, superclassOpt, li) }

bloc:
    | l = delimited(LBRACE, list(instruction), RBRACE)                                          {l }
    | LBRACE var = separated_nonempty_list(COMMA, methodParameter) IS
      li = nonempty_list(instruction) RBRACE                                                    {var, li }

expression:
    // Identificateur
    | x = ID                                                                                    { Id x }
    // Constante entière
    | v = CSTE                                                                                  { Cste v }
    // String
    | s = STRING                                                                                { }
    // (Expression)
    | e = delimited(LPAREN, expression, RPAREN)                                                 { e }
    // (NomClasse Expression)
    | LPAREN x = CLASSNAME e = expression RPAREN                                                { }
    // Sélection / Envoi de messages
    | e = expression DOT m = ID                                                                 { }
    | e = expression DOT m = ID
      lparam = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)                     { }
    // Instanciation
    | NEW x = CLASSNAME lparam = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)   { }
    // Expression avec opérateur
    | g = expression op = RELOP d = expression                                                  { Comp(op, g, d) }
    | g = expression PLUS d = expression                                                        { Plus (g, d) }
    | g = expression MINUS d = expression                                                       { Minus(g, d) }
    | g = expression TIMES d = expression                                                       { Times(g, d) }
    | g = expression DIV d = expression                                                         { Div(g, d) }
    | g = expression CONCAT d = expression                                                      { }
    | PLUS e = expression  %prec UMINUS                                                         { e }
    | MINUS e = expression %prec UMINUS                                                         { UMinus e }

instruction:
    // Expression;
    | e = expression SEMICOLON                                                                  { e }
    // bloc
    | b = bloc                                                                                  { b }
    // return;
    | RETURN SEMICOLON                                                                          { Return }
    // cible := Expression
    | e1 = expression ASSIGN e2 = expression SEMICOLON                                          { AffectationField(e1, e2) }
    // if Expression then Instruction else Instruction
    | IF si = expression THEN alors = instruction ELSE sinon = instruction                      { Ite(si, alors, sinon) }
