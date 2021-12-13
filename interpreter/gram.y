%{

/*
 *	   ___    0  Michel Beaudouin-Lafon        
 *	  /   \  /   LRI - Bat 490                 e-mail: mbl@lri.fr
 *	 /  __/ /    Universite de Paris-Sud       voice : +33 (1) 69 41 69 10
 *	/__   \/     91 405 ORSAY Cedex - FRANCE   fax   : +33 (1) 69 41 65 86
 *
 *	(c) Copyright 1992
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "interp.h"

extern int yylex();
extern void yyerror(char *s);
extern void lexError (char *s, void *arg);

extern void Run(), RecordOp();
void  RecordLabel(char *name);
void ResolveLabels(), RecordOp(int brk, int op, long a1, long a2, char *as, char *com);

static	long	sizeprog = 200;
static	instr*	maxprog = 0;
extern int yylineno;

%}

%union{
	long	intval;
	char*	strval;
}

%token	<intval>	OP0 OP1 OP2 OPS OPL OPC
%token	<intval>	INT
%token	<strval>	NAME LABEL
%token	<strval>	STRING EOL
%token			BREAK
%token			','

%type	<intval>	pre break

%%

prog	: linstr			{ ResolveLabels (); }
;

linstr	: /*empty*/
	| linstr inst
;

inst	: pre OP0 EOL			{ RecordOp ($1, $2, 0, 0, 0, $3); }
	| pre OP1 INT EOL		{ RecordOp ($1, $2, $3, 0, 0, $4); }
	| pre OPS STRING EOL		{ RecordOp ($1, $2, 0, 0, $3, $4); }
	| pre OPL NAME EOL		{ RecordOp ($1, $2, 0, 0, $3, $4); }
	| EOL				{ }
	;

pre	: break label			{ $$ = $1; }
	;

break	: /*empty*/			{ $$ = 0; }
	| BREAK				{ $$ = 1; }
	;

label	: /*empty*/
	| LABEL				{ RecordLabel ($1); }
	;
%%

void RecordOp(int brk, int op, long a1, long a2, char *as, char *com) {
  if (endprog >= maxprog) {
    if (! prog) {
      prog = (instr*) malloc (sizeprog * sizeof (instr));
      endprog = prog;
    } else {
      long	nbinstr = endprog - prog;

      sizeprog *= 2;
      prog = (instr*) realloc (prog, sizeprog * sizeof (instr));
      endprog = prog + nbinstr;
    }
    maxprog = prog + sizeprog;
  }
  endprog->brk = brk;
  endprog->lineno = yylineno;
  endprog->op = op;
  endprog->a1 = a1;
  endprog->a2 = a2;
  endprog->str = as;
  endprog->branch = 0;
  endprog->comment = com;
  endprog++;
}

typedef struct _label {
  struct _label *next;
  char *name;
  long address;
} label;

label *labels = 0;

label *GetLabel(char *name) {
  label *l;

  for (l = labels; l; l = l->next)
    if (strcmp (name, l->name) == 0) return l;
  return 0;
}

char *GetLabelName(instr *i) {
  label *l;
  long addr = i - prog;

  for (l = labels; l; l = l->next)
    if (addr == l->address) return l->name;
  return 0;
}

void RecordLabel(char *name) {
  label *l;

  if (GetLabel (name)) {
    lexError ("label %s redefined", name);
    return;
  }
  l = New (label);
  l->next = labels;
  labels = l;
  l->name = name;
  l->address = endprog - prog;
}

void ResolveLabels() {
  instr *i;
  label *l;

  for (i = prog; i < endprog; i++) {
    switch (i->op) {
    case JUMP:
    case JZ:
    case PUSHA:
	l = GetLabel (i->str);
	if (! l) lexError ("undefined label %s", i->str);
	else i->branch = prog + l->address;
    }
  }
}

int main(int argc, char** argv) {
  extern	int	errors;
  extern	int	debug;
  extern	FILE*	yyin;
  char*	progname = argv [0];
  char*	file = 0;
  int res;
	
  while (--argc > 0) {
    ++argv;
    if (argv [0][0] == '-') {
      switch (argv [0][1]) {
      case 'd':
	debug = 1;
	break;
      default:
	fprintf (stderr, "usage: %s [-d] [file]\n", progname);
	exit (1);
      }
    } else {
      if (file) {
	fprintf (stderr, "usage: %s [-d] [file]\n", progname);
	exit (1);
      } else file = argv [0];
    }
  }

  if (file) {
    yyin = fopen (file, "r");
    if (yyin == 0) {
      fprintf (stderr, "%s: %s: cannot open\n", progname, file);
      exit (2);
    }
  }

  res = yyparse ();

  if (errors) {
    fprintf (stderr, "%d errors - program not run\n", errors);
    exit (99);
  }

  /* apres, car peut-etre le message d'avant donne pus d'info. */
  if (res != 0) {
    fprintf (stderr, "Program not run\n");
    exit (99);
  }

  Run ();
  return 0;
}
