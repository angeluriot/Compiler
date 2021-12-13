/*
 *	   ___    0  Michel Beaudouin-Lafon        
 *	  /   \  /   LRI - Bat 490                 e-mail: mbl@lri.fr
 *	 /  __/ /    Universite de Paris-Sud       voice : +33 (1) 69 41 69 10
 *	/__   \/     91 405 ORSAY Cedex - FRANCE   fax   : +33 (1) 69 41 65 86
 *
 *	(c) Copyright 1992
 */

#define	New(type)	(type*) malloc (sizeof (type))

typedef struct _instr {
	short	brk;
  	short lineno;
	short	op;
	long	a1;
	long	a2;
	char*	str;
	char*	comment;
	struct _instr*	branch;
} instr;

extern	instr*	prog;
extern	instr*	endprog;

typedef struct {
	char*	name;
	int	code;
	int	token;
} opcode;

extern	opcode	opcodes [];

#define	NOP	0
#define	ERR	1
#define	PUSHI	2
#define	PUSHS	3
#define	PUSHG	4
#define	PUSHL	5
#define	PUSHN	6
#define	POPN	7
#define STOREL	8
#define STOREG	9
#define	DUPN	10
#define	JUMP	11
#define	JZ	12
#define PUSHA	13
#define	CALL	14
#define	RET	15
#define	ADD	16
#define	CONCAT	17
#define	SUB	18
#define	MUL	19
#define	DIV	20
#define	INF	21
#define	INFEQ	22
#define	SUP	23
#define	SUPEQ	24
#define	WRITEI	25
#define	WRITES	26
#define	READ	27
#define	ATOI	28
#define	STR	29
#define	EQUAL	30
#define	NOT	31
#define	STORE	32
#define	LOAD	33
#define	START	34
#define	STOP	35
#define ALLOC	36
#define SWAP	37
