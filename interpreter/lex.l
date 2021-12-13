%{

/*
 *	   ___    0  Michel Beaudouin-Lafon        
 *	  /   \  /   LRI - Bat 490                 e-mail: mbl@lri.fr
 *	 /  __/ /    Universite de Paris-Sud       voice : +33 (1) 69 41 69 10
 *	/__   \/     91 405 ORSAY Cedex - FRANCE   fax   : +33 (1) 69 41 65 86
 *
 *	(c) Copyright 1992
 */
#include <string.h>
#include "interp.h"
#include "gram.h"

extern	char *String (char *);
extern	void FreeString (char *);
void lexError (char *s, char *arg);
extern void yyerror (char *s);

int Opcode(char *);

opcode opcodes [] = {
	{ "NOP",	NOP,	OP0 },
	{ "ERR",	ERR,	OPS },
	{ "PUSHI",	PUSHI,	OP1 },
	{ "PUSHS",	PUSHS,	OPS },
	{ "PUSHG",	PUSHG,	OP1 },
	{ "PUSHL",	PUSHL,	OP1 },
	{ "PUSHN",	PUSHN,	OP1 },
	{ "POPN",	POPN,	OP1 },
	{ "STOREL",	STOREL,	OP1 },
	{ "STOREG",	STOREG,	OP1 },
	{ "DUPN",	DUPN,	OP1 },
	{ "JUMP",	JUMP,	OPL },
	{ "JZ",		JZ,	OPL },
	{ "PUSHA",	PUSHA,	OPL },
	{ "CALL",	CALL,	OP0 },
	{ "RETURN",	RET,	OP0 },
	{ "ADD",	ADD,	OP0 },
	{ "CONCAT",	CONCAT, OP0 },
	{ "SUB",	SUB,	OP0 },
	{ "MUL",	MUL,	OP0 },
	{ "DIV",	DIV,	OP0 },
	{ "INF",	INF,	OP0 },
	{ "INFEQ",	INFEQ,	OP0 },
	{ "SUP",	SUP,	OP0 },
	{ "SUPEQ",	SUPEQ,	OP0 },
	{ "WRITEI",	WRITEI,	OP0 },
	{ "WRITES",	WRITES, OP0 },
/*	{ "READ",	READ,	OP0 }, */
/*	{ "ATOI",	ATOI,	OP0 }, */
	{ "STR",	STR,	OP0 },
	{ "EQUAL",	EQUAL,	OP0 },
	{ "NOT",	NOT,	OP0 },
	{ "STORE",	STORE,	OP1 },
	{ "LOAD",	LOAD,	OP1 },
	{ "START",	START,	OP0 },
	{ "STOP",	STOP,	OP0 },
	{ "ALLOC",	ALLOC,	OP1 },
	{ "SWAP",	SWAP,	OP0 },
	{ 0,		0,	OP0 }
};

%}
%option yylineno

word    [_a-zA-Z]+[_a-zA-Z0-9]*
num     [0-9]+

%%

[+-]?{num}	{
		yylval.intval = atoi (yytext);
		return INT;
	}

{word}	{
		return Opcode (yytext);
	}

{word}:	{
		yytext [yyleng -1] = 0;	/* enlever ':' final */
		yylval.strval = strdup(yytext);
		return LABEL;
	}

\"([^\\\n"]|\\.)*\"	{
		yylval.strval = String (yytext);
		return STRING;
	}

\-\-.*\n	{
		yytext [yyleng -1] = 0; 	/* get rid of trailing \n */
		yylval.strval = strdup(yytext);
		return EOL;
	}

\*	{	return BREAK;	}

[ \t]	{	/* delimiters */	}

[\n]	{	yylval.strval = 0; return EOL;	}

.	{	yytext [1] = '\0'; lexError ("unknown character %s", yytext);	}

%%

int	errors = 0;

void lexError (char *s, char *arg) {
  fprintf (stderr, "*** line %d: ", yylineno);
  fprintf (stderr, s, arg);
  fprintf (stderr, "\n");
  errors++;
}

int Opcode (char *s) {
  opcode *op;

  for(op = opcodes; op->name; op++)
     if (strcmp (op->name, s) == 0) {
	yylval.intval = op->code;
	return op->token;
     }
  yylval.strval = strdup(s);
  return NAME;
}

char* String (char *s) {
  char buf [1024];
  char* p = buf;

  s++;	/* sauter " initiale */
  while (*s) {
    if (*s == '\\') {
       switch (*++s) {
	 case 'n': *p++ = '\n'; break;
	 case 't': *p++ = '\t'; break;
	 default:  *p++ = *s;  break;
       }
       s++;
    } else *p++ = *s++;
  }
  *(p-1) = 0;	/* ignorer " finale */
  return strdup(buf);
}

void FreeString (char *s) {
  if (s) free (s);
}
