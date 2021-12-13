#	   ___    0  Michel Beaudouin-Lafon        
#	  /   \  /   LRI - Bat 490                 e-mail: mbl@lri.fr
#	 /  __/ /    Universite de Paris-Sud       voice : +33 (1) 69 41 69 10
#	/__   \/     91 405 ORSAY Cedex - FRANCE   fax   : +33 (1) 69 41 65 86
#
#	(c) Copyright 1992

OBJ=gram.o lex.o interp.o
CC=gcc
CFLAGS= -Wall -Wno-unused-but-set-variable -Wno-unused-function -Wno-unknown-warning-option -c -I./ -g 
LDFLAGS= -g -lfl -ly
interp : $(OBJ)
	$(CC) -o interp $(OBJ) $(LDFLAGS)

gram.o : gram.y interp.h
	bison -b gram -o gram.c -d gram.y
	$(CC) $(CFLAGS) -DYYDEBUG -c gram.c

gram.h: gram.y
	bison -b gram -o gram.c -d gram.y

lex.o : lex.l interp.h gram.h
	flex --yylineno -olex.c lex.l
	$(CC) $(CFLAGS) -c lex.c

interp.o : interp.h gram.h

clean:	
	rm -f *.o lex.c gram.c gram.output gram.h interp

