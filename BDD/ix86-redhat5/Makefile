BDD = ../bdd/src
MU = ../mu/src
UTILS = ../bdd/utils
INCLUDES = -I$(BDD) -I$(UTILS) -I$(MU)
LD = $(CC)
LDFLAGS = -shared -L./
CC = gcc
CFLAGS = -D_POSIX_SOURCE -DSYSV $(INCLUDES)
XCFLAGS = -O
SHELL = /bin/sh
VPATH = ..:../bdd/utils:../bdd/src:../mu/src

bddobj = bdd_interface.o bdd_fns.o bdd.o bdd_factor.o bdd_quant.o bdd_vfns.o

muobj = bdd_interface.o bdd.o bdd_factor.o bdd_quant.o bdd_fns.o bdd_vfns.o \
        appl.o mu_interface.o mu.o

utilobj = double.o list.o hash.o alloc.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : bdd.so mu.so

bdd.so : libutils.a ${bddobj}
	$(LD) ../bdd-ld-table $(LDFLAGS) -o bdd.so ${bddobj} -lutils -lm

mu.so : ${muobj}
	$(LD) $(LDFLAGS) -o  mu.so  ${obj} -lutils  -lm 

utils : libutils.a

libutils.a : ${utilobj}
	ar r libutils.a ${utilobj}
	ranlib libutils.a

bdd_interface.o : bdd_interface.c bdd_fns.h
bdd_factor.o : bdd_factor.c bdd_factor.h
bdd.o : bdd.c bdd.h bdd_extern.h
bdd_fns.o : bdd_fns.c bdd_fns.h bdd.h bdd_extern.h
bdd_quant.o : bdd_quant.c bdd_fns.h bdd.h bdd_extern.h
bdd_vfns.o : bdd_vfns.c bdd_vfns.h bdd_fns.h bdd.h bdd_extern.h

mu_interface.o : mu_interface.c mu.h
mu.o : mu.c mu.h

double.o : double.c double.h
list.o : list.c list.h alloc.h
hash.o : hash.c hash.h alloc.h
alloc.o : alloc.c

clean : 
	rm -f *.o *.a *.so

