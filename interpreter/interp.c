/*
 *	   ___    0  Michel Beaudouin-Lafon        
 *	  /   \  /   LRI - Bat 490                 e-mail: mbl@lri.fr
 *	 /  __/ /    Universite de Paris-Sud       voice : +33 (1) 69 41 69 10
 *	/__   \/     91 405 ORSAY Cedex - FRANCE   fax   : +33 (1) 69 41 65 86
 *
 *	(c) Copyright 1992
 */


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include "interp.h"
#include "gram.h"


extern char *GetLabelName();
extern char *strdup(const char *);

extern int yylineno;

void yyerror(char *ignore) {
  fprintf(stderr, "Syntax error on line: %d\n", yylineno);
}

void Error(char *msg);

int debug = 0;
int trace = 0;
int stepping = 0;
int interrupt = 0;

/*
 *	pile d'execution
 */
typedef	long	data;
#define MAXSTACK	32000
data	stack	[MAXSTACK];
data *endstack = stack + MAXSTACK;
data *sp = stack;
data *savedsp = stack;	/* sauvegarde pointeur de pile pour Error */
data *fp = 0;

/*
 *	programme
 */
instr *prog = 0;
instr *endprog = 0;
instr *pc;

/*
 *	liste des appels de fonctions en cours
 */
typedef struct _frame {
  struct _frame *next;
  instr *ret;
  data *fp;
} frame;

frame *callstack = 0;

/*
 *	liste des chaines
 *	note: les chaines allouees ne sont jamais liberees.
 *	Il faudrait gerer un compte de reference, et savoir
 *	quels elements de la pile sont des chaines, lorsque l'on depile.
 */
typedef struct _string {
  struct _string *next;
  char *value;
} string;

string *stringlist = 0;

/*
 *	liste des objets
 *	note: les objets allouees ne sont jamais liberees.
 *	Il faudrait gerer un compte de reference, et savoir
 *	quels elements de la pile sont des objets, lorsque l'on depile.
 */
typedef struct _object {
  struct _object*  next;
  long id;
  char *name;
  long size;
  long *fields;
} object;

object *objectlist = 0;

/*
 *	dump d'une instruction
 *	dump de la pile
 *	dump des objets
 */
void DumpInstr (instr *i) {
  opcode *op;
  char *label;

  /* mettre une * si c'est un point d'arret */
  if (i->brk) fprintf (stderr, "*");

  /* afficher le numero d'instruction et le label eventuel */
  label = GetLabelName (i);
  /* les lignes sont numerotees a partir de 1 dans les editeurs de texte */
  if (label) fprintf (stderr, "%d: %s:	", i->lineno-1, label);
  else fprintf (stderr, "%d:	", i->lineno-1);

  /* afficher le mnemonique */
  for (op = opcodes; op->name; op++)
    if (op->code == i->op) break;
  if (! op->name) fprintf (stderr, "???\n");
  fprintf (stderr, "%s	", op->name);

  /* afficher les arguments en fonction de son type */
  switch (op->token) {
  case OP0:
    break;
  case OP1:
    fprintf (stderr, "%ld", i->a1);
    break;
  case OPL:
    fprintf (stderr, "%s", i->str);
    break;
  case OPS:
    fprintf (stderr, "\"%s\"", i->str);
    break;
  }
  /* afficher le commentaire associe s'il y en a un */
  if (i->comment) fprintf (stderr, "\t%s", i->comment);
  fprintf (stderr, "\n");
}

void PrintSym (long *addr) {
  object *obj;
  string *str;
  instr *i;

  /* si ca ressemble a un objet, l'imprimer */
  for (obj = objectlist; obj; obj = obj->next) {
    if (addr >= obj->fields && addr < (obj->fields + obj->size)) {
      int i = addr - obj->fields;
      fprintf (stderr, "\tobj#%ld", obj->id);
      if (i > 0) fprintf (stderr, "[%d]", i);
      if (obj->name) fprintf (stderr, "\t%s", obj->name);
      return;
    }
  }
  /* si ca ressemble a une string, l'imprimer */
  for (str = stringlist; str; str = str->next)
    if (addr == (long*) str->value) {
      fprintf (stderr, "\t\"%s\"", str->value);
      return;
    }
  /* si ca ressemble a un label l'imprimer */
  i = (instr*) addr;
  if (i >= prog && i < endprog) {
    /* afficher le numero d'instruction et le label eventuel */
    char *label = GetLabelName (i);
    fprintf (stderr, "\tinstr %ld", i - prog);
    if (label) fprintf (stderr, " (%s)", label);
  }
}
		
void DumpStack (void) {
  data *p;
  frame *f = callstack;

  if (fp == sp) fprintf(stderr, "fp>\t|\n");
  for (p = sp - 1; p >= stack; --p) {
    /* est-ce le fp courant ? */
    if (fp == p) fprintf (stderr, "fp>");
    /* est-ce un fp precedent ? */
    if (f && f->fp == p) {
      fprintf (stderr, "(fp)");
      f = f->next;
    }
    /* imprimer l'element de pile */
    fprintf (stderr, "\t| %ld", *p);
    PrintSym ((long *) *p);
    fprintf (stderr, "\n");
  }
  /* le fond de la pile */
  fprintf (stderr, "\t|________\n");
}

void DumpObject (object *obj) {
  int i;

  fprintf (stderr, "obj#%ld, address %ld", obj->id, (long) obj->fields);
  if (obj->name) fprintf (stderr, "\t%s", obj->name);
  fprintf (stderr, "\n");
  for (i = 0; i < obj->size; i++) {
  /* imprime la valeur brute de chaque champ, puis essaie de
   * l'interpreter !
   */
    fprintf (stderr, "\t[%02d] %ld", i, *(obj->fields + i));
    PrintSym ((long *) *(obj->fields + i));
    fprintf (stderr, "\n");
  }
}

void DumpObjects () {
  object *obj;

  fprintf (stderr, "\n");
  for (obj = objectlist; obj; obj = obj->next)
    DumpObject (obj);
  fprintf (stderr, "\n");
}

void Debug (instr *i) {
  static char c = '?';	/* derniere commande */
  char readbuf [1024];

  for (;;) {
    fprintf (stderr, ">>> ");
    if (fgets(readbuf, 1024, stdin) == NULL) {
      fprintf (stderr, "au revoir\n");
      exit (0);
    }
    if (interrupt) {
      fprintf (stderr, ">>> Interruption utilisateur\n");
      interrupt = 0;
      continue;
    }
    if (readbuf [0] != '\n')
      c = readbuf [0];
    switch (c) {
    case '?' :
      fprintf (stderr, "	? 	afficher ce message\n");
      fprintf (stderr, "	a	poser un point d'arret\n");
      fprintf (stderr, "	d	detruire un point d'arret\n");
      fprintf (stderr, "	c	continuer\n");
      fprintf (stderr, "	i	avancer d'une instruction\n");
      fprintf (stderr, "	p	afficher la pile\n");
      fprintf (stderr, "	o	afficher les objets\n");
      fprintf (stderr, "	+	voir l'instruction suivante\n");
      fprintf (stderr, "	-	voir l'instruction precedente\n");
      fprintf (stderr, "	t	activer / desactiver la trace\n");
      fprintf (stderr, "	q	quitter\n");
      fprintf (stderr, "	RC	repeter la derniere commande\n");
      break;
    case 'a' :
      i->brk = 1;
      DumpInstr (i);
      break;
    case 'd' :
      i->brk = 0;
      DumpInstr (i);
      break;
    case 'p' :
      DumpStack ();
      break;
    case 'o' :
      if (readbuf [1]) {
	int n = atoi (readbuf+2);
	object *obj;

	if (! n) {
	  fprintf (stderr, "syntaxe: o addresse d'objet\n");
	  break;
	}
	for (obj = objectlist; obj; obj = obj->next) {
	  if (obj->id == n) {
	    DumpObject (obj);
	    break;
	  }
	}
	if (! obj)
	  fprintf (stderr, "pas d'objet numero %d\n", n);
      } else DumpObjects ();
      break;
    case '+' :
      if (i < endprog) {
	i++;
	DumpInstr (i);
      } else fprintf (stderr, "fin du programme\n");
      break;
    case '-' :
      if (i > prog) {
	i--;
	DumpInstr (i);
      } else fprintf (stderr, "debut du programme\n");
      break;
    case 'c' :
      stepping = 0;
      return;
    case 'i' :
      stepping = 1;
      return;
    case 't' :
      trace = 1 - trace;
      fprintf (stderr, "trace %s\n", trace ? "active" : "inactive");
      break;
    case 'q' :
      fprintf (stderr, "au revoir\n");
      exit (0);
      break;
    default :
      fprintf (stderr, "commande inconnue. taper ? pour la liste\n");
      break;
    }
  }
}

/*
 *	ajout d'une chaine, controle de la validite d'une chaine
 *	note: les chaines ne sont jamais desallouees...
 */
char * AddString (char *s) {
  string *str;

  /* si la chaine est deja dans la liste, il est inutile de la reallouer */
  for (str = stringlist; str; str = str->next)
    if (strcmp (str->value, s) == 0) return str->value;
  str = New (string);
  str->next = stringlist;
  stringlist = str;
  str->value = s;
  return s;
}

void CheckString (char *s) {
  string *str;

  for (str = stringlist; str; str = str->next)
    if (str->value == s) return;
  Error ("chaine de caracteres invalide");
}

/*
 *	allocation d'un objet, controle de la validite d'un acces a un objet
 *	note: les objets ne sont jamais desalloues...
 */
long *AllocObject (int nflds, char *name) {
  static int objid = 0;
  object *obj;

  obj = (object*) malloc (sizeof (object));
  obj->next = objectlist;
  objectlist = obj;
  obj->id = ++objid;
  obj->name = name;
  obj->size = nflds;
/*  obj->fields = (long *) calloc(nflds * sizeof (long)); */
  obj->fields = (long *) malloc(nflds * sizeof (long));
  return obj->fields;
}

void CheckObject (long *addr) {
  object*	obj;

  for (obj = objectlist; obj; obj = obj->next) {
    int i;
    /* tester qu'on est dans l'objet */
    if (addr >= obj->fields && addr < (obj->fields + obj->size)) {
      /* tester que c'est une adresse bien alignee */
      for (i = 0; i < obj->size; i++)
	if (addr == (obj->fields + i)) return;
      Error ("reference invalide a un objet: adresse non alignee");
      return;
    }
  }
  Error ("reference invalide a un objet: objet inexistant");
}

/*
 *	appel et retour de fonction
 */
void Call(instr *i) {
  frame *f;

  f = New (frame);
  f->next = callstack;
  callstack = f;
  f->fp = fp;
  f->ret = pc;
  fp = sp;
  if (f->fp < stack) Error ("Mauvais appel a CALL"); 
  pc = i;
}

void Return() {
  frame *f = callstack;

  if (! f || ! fp) Error ("RETURN sans CALL prealable");
  sp = fp;
  fp = f->fp;
  pc = f->ret;
  callstack = f->next;
  free (f);
}

/*
 *	manipulations de pile
 */
void CheckStack (data *ps, long n) {
  if (ps < stack || ps + n > endstack)
    Error ("adresse en dehors de la pile");
}

void CheckPush (long n) {
  if (n < 0) Error ("taille negative");
  if (sp + n > endstack) Error ("debordement de pile");
}

void CheckPop (long n) {
  if (n < 0) Error ("taille negative");
  if (sp - n < stack) Error ("pile vide");
}

void CopyStack (data *from, data *to, long len) {
  if (len < 0) Error ("nombre negatif de valeurs a copier");
  CheckStack (from, len);
  CheckStack (to, len);
  while (len--)	*to++ = *from++;
}

char * itoa (long n) {
  char a [128];

  sprintf (a, "%ld", n);
  return AddString(strdup(a));
}

char* Concat (char *s1,	char *s2) {
  char *r;
	
  CheckString (s1);
  CheckString (s2);
  r = (char *) malloc (strlen (s1) + strlen (s2) + 1);
  strcpy (r, s1);
  strcat (r, s2);
  return AddString (r);
}

/*
 *	execution
 */

void sigint () {
  interrupt = 1;
  signal (2, sigint);
  signal (3, sigint);
}

void Run () {
  long	i, n; long *addr; 
  /* char *s, readbuf [2048]; */

  /* attraper le ^C */
  signal (2, sigint);
  signal (3, sigint);

  pc = prog;
  if (debug) {
    trace = 1;
    stepping = 1;
    fprintf (stderr, "taper ? pour la liste des commandes\n");
    Debug (pc);
  }

  for (;;) {
    savedsp = sp;
    if (pc == endprog) Error ("fin prematuree du programme");

    /* gestion des interruptions, points d'arret, et mode debug */
    if (interrupt || pc->brk || stepping || trace) {
      DumpObjects ();
      DumpStack ();
      DumpInstr (pc);
    }
    if (interrupt)
      fprintf (stderr, ">>> Interruption utilisateur\n");
    else if (pc->brk)
      fprintf (stderr, ">>> Point d'arret\n");
    if (interrupt || pc->brk || stepping)
      Debug (pc);
    interrupt = 0;

    /* execution d'une instruction */
    switch (pc->op) {
    case NOP :
      break;
    case ERR :
      Error (pc->str);
      break;
    case PUSHI :
      CheckPush (1);
      *sp++ = pc->a1;
      break;
    case PUSHS :
      CheckPush (1);
      *sp++ = (data) AddString (pc->str);
      break;
    case PUSHG :
      CheckPush (1);
      CopyStack (stack + pc->a1, sp, 1);
      sp++;
      break;
    case PUSHL :
      CheckPush (1);
      if (! fp)
	Error ("reference a des variables locales avant START");
      CopyStack (fp + pc->a1, sp, 1);
      sp++;
      break;
    case PUSHN :
      CheckPush (pc->a1);
      for (i = pc->a1; i > 0; i--)
	*sp++ = 0;
      break;
    case POPN :
      CheckPop (pc->a1);
      sp -= pc->a1;
      break;
    case STOREL :
      CheckPop (1);
      if (! fp)
	Error ("reference a des variables locales avant START");
      n = *--sp;
      CopyStack (sp, fp + pc->a1, 1);
      break;
    case STOREG :
      CheckPop (1);
      n = *--sp;
      CopyStack (sp, stack + pc->a1, 1);
      break;
    case DUPN :
      CheckPush (pc->a1);
      CopyStack (sp - pc->a1, sp, pc->a1);
      sp += pc->a1;
      break;
    case JUMP :
      pc = pc->branch -1;	/* incremente a la fin */
      break;
    case JZ :
      CheckPop (1);
      n = *--sp;
      if (n == 0)
	pc = pc->branch -1;	/* incremente a la fin */
      break;
    case CALL :
      CheckPop (1);
      n = *--sp;
      Call ((instr *) n);
      --pc;	/* car incremente a la fin */
      break;
    case RET :
      Return();
      break;
    case ADD :
      CheckPop (2);
      --sp;
      *(sp-1) += *sp;
      break;
    case CONCAT :
      CheckPop (2);
      --sp;
      *(sp-1) = (data) Concat ((char *) *(sp-1), (char *) *sp);
      break;
    case SUB :
      CheckPop (2);
      --sp;
      *(sp-1) -= *sp;
      break;
    case MUL :
      CheckPop (2);
      --sp;
      *(sp-1) *= *sp;
      break;
    case DIV :
      CheckPop (2);
      --sp;
      if (*sp == 0)
	Error ("division par zero");
      *(sp-1) /= *sp;
      break;
    case INF :
      CheckPop (2);
      --sp;
      *(sp-1) = (*(sp -1) < *sp) ? 1 : 0;
      break;
    case INFEQ :
      CheckPop (2);
      --sp;
      *(sp-1) = (*(sp -1) <= *sp) ? 1 : 0;
      break;
    case SUP :
      CheckPop (2);
      --sp;
      *(sp-1) = (*(sp -1) > *sp) ? 1 : 0;
      break;
    case SUPEQ :
      CheckPop (2);
      --sp;
      *(sp-1) = (*(sp -1) >= *sp) ? 1 : 0;
      break;
    case WRITEI :
      CheckPop (1);
      --sp;
      printf ("%ld", *sp);
      break;
    case WRITES :
      CheckPop (1);
      CheckString ((char*) *(sp -1));
      printf ("%s", (char *) *--sp);
      break;
/*
    case READ :
      CheckPush (1);
      if (! gets (readbuf))
	Error ("erreur de lecture dans READ");
      *sp++ = (data) AddString (strdup(readbuf));
      break;
*/
/*
    case ATOI :
      CheckPop (1);
      s = (char*) *(sp -1);
      CheckString (s);
      n = atoi (s);
      if (n == 0 && strcmp (s, "0") != 0)
	Error ("valeur incorrecte pour ATOI : \"%s\"", s);
      *(sp-1) = n;
      break;
*/
    case STR :
      CheckPop (1);
      *(sp-1) = (data) itoa (*(sp -1));
      break;
    case EQUAL :
      CheckPop (2);
      sp -= 2;
      if (*sp == *(sp+1)) *sp++ = 1;
      else *sp++ = 0;
      break;
    case NOT :
      CheckPop (1);
      *(sp -1) = *(sp -1) ? 0 : 1;
      break;
    case STORE :
      CheckPop (2);
      n = *--sp;
      addr = ((long*) *--sp) + pc->a1;
      CheckObject (addr);
      *addr = n;
      break;
    case LOAD :
      CheckPop (1);
      addr = ((long*) *--sp) + pc->a1;
      CheckObject (addr);
      *sp++ = *addr;
      break;
    case START :
      if (fp) Error ("deuxieme occurence de START");
      fp = sp;
      break;
    case STOP :
      exit (0);
      break;
    case ALLOC :
      CheckPush (1);
      *sp++ = (long) AllocObject (pc->a1, pc->comment);
      break;
    case PUSHA :
      CheckPush (1);
      *sp++ = (long) pc->branch;
      break;
    case SWAP :
      CheckPop (2);
      n = *(sp-1);
      *(sp-1) = *(sp-2);
      *(sp-2) = n;
      break;
    }
    pc++;
  }
}

void Error(char *msg) {
  char buf[1024];
  data *oldsp;

  sprintf (buf, "### erreur : %s\n", msg);
  fprintf (stderr,"%s", buf);
  DumpInstr (pc);

  /* afficher la pile jusqu'au sp sauve' */
  oldsp = sp;
  sp = savedsp;
  DumpStack ();
  sp = oldsp;
  exit (1);
}
