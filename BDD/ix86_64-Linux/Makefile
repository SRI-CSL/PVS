BDD = ../bdd/src
MU = ../mu/src
UTILS = ../bdd/utils
INCLUDES = -I/usr/include -I$(BDD) -I$(UTILS) -I$(MU)
LD = ld
LDFLAGS = -Bsymbolic -shared -warn-once -L./
CC = gcc
CFLAGS = -D_POSIX_SOURCE -DSYSV $(INCLUDES) -DLINUX -DLINUX_REDHAT5 -DSIGNALS_LINUX -fPIC
XCFLAGS = -O
SHELL = /bin/sh
VPATH = ..:../bdd/utils:../bdd/src:../mu/src

muobj = bdd_interface.o bdd.o bdd_factor.o bdd_quant.o bdd_fns.o bdd_vfns.o \
        appl.o mu_interface.o mu.o

utilobj = double.o list.o hash.o alloc.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : mu.so

mu.so : ${muobj} libutils.a ../bdd-ld-table ../mu-ld-table 
	$(LD) ../bdd-ld-table ../mu-ld-table $(LDFLAGS) -o  mu.so  ${muobj} -lutils  -lm -lbsd

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

