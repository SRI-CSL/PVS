# Makefile for Reduced Ordered Boolean Decision Diagram Manipulation
# includes: lexical analyser + parser
# Copyright (c) 1992-1998 G. Janssen

UTILS=../utils

INCLUDES   = -I. -I$(UTILS)

ARS = $(UTILS)/libutils.a

# specify appropriate C compiler flags here:
#CFLAGS=  -Aa -D_POSIX_SOURCE $(INCLUDES)
#CFLAGS= -D_POSIX_SOURCE $(INCLUDES)
CFLAGS= -g -O $(INCLUDES)
XCFLAGS= -O
XLDFLAGS=-s

obj= bdd_factor.o bdd.o bdd_quant.o bdd_fns.o bdd_vfns.o appl.o y.tab.o \
     lex.yy.o main.o

bdd : ${obj}
	$(CC) ${XLDFLAGS} -o bdd ${obj} $(ARS) -lfl -lm

template : lib template.o
	$(CC) -o template template.o libbdd.a $(ARS)

.c.o : ; $(CC) ${XCFLAGS} ${CFLAGS} -c $*.c

bdd_factor.o : bdd_factor.c bdd_factor.h
bdd.o : bdd.c bdd.h bdd_extern.h
bdd_fns.o : bdd_fns.c bdd_fns.h bdd.h bdd_extern.h
bdd_quant.o : bdd_quant.c bdd_fns.h bdd.h bdd_extern.h
bdd_vfns.o : bdd_vfns.c bdd_vfns.h bdd_fns.h bdd.h bdd_extern.h
appl.o : appl.c appl.h bdd_fns.h bdd.h bdd_extern.h
main.o : main.c appl.h bdd_fns.h bdd.h bdd_extern.h
y.tab.c : yacc.y bdd_fns.h bdd.h bdd_extern.h
	# Don't worry about the 1 shift/reduce conflicts!
	yacc -d yacc.y
y.tab.h : yacc.y
	yacc -d yacc.y
	# Don't worry about the 1 shift/reduce conflicts!
y.tab.o : y.tab.c bdd_fns.h bdd.h bdd_extern.h
lex.yy.c : lex.l
	lex lex.l
lex.yy.o : lex.yy.c y.tab.h bdd.h bdd_extern.h
	$(CC) -c ${CFLAGS} -w lex.yy.c

clean : 
	rm -f *.o *.a lex.yy.c y.tab.c y.tab.h bdd template

lib : libbdd.a

libbdd.a : bdd.o bdd_fns.o bdd_quant.o
	ar r libbdd.a bdd.o bdd_fns.o bdd_quant.o
	ranlib libbdd.a

lib2 : libbdd2.a

libbdd2.a : bdd.o bdd_fns.o bdd_quant.o bdd_factor.o bdd_vfns.o appl.o y.tab.o \
            lex.yy.o
	ar r libbdd2.a bdd.o bdd_fns.o bdd_quant.o bdd_factor.o bdd_vfns.o \
             appl.o y.tab.o lex.yy.o
	ranlib libbdd2.a

all:	bdd lib lib2

install: all
	cp *.h /usr/es/include/bdd
	cp *.a /usr/es/lib
	mv bdd /usr/es/bin

install_akebono: all
	cp *.h /opt/es/include/bdd
	cp *.a /opt/es/lib
	mv bdd /opt/es/bin

what :
	# make       - makes the bdd program
	# make bdd   - makes the bdd program
	# make lib   - creates the archive libbdd.a
	# make lib2  - creates the archive libbdd2.a
	# make template - creates the "template" example program
	# make clean - cleans up objects and archives in this directory
