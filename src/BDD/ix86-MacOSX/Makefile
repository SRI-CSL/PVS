BDD = ../bdd/src
MU = ../mu/src
UTILS = ../bdd/utils
INCLUDES = -I/usr/include -I$(BDD) -I$(UTILS) -I$(MU)
# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for both leopard and tiger
MACOSX_DEPLOYMENT_TARGET=10.11
LD = ld
SDK=$(shell xcrun --show-sdk-path)
LDFLAGS = -bundle -flat_namespace -undefined suppress -L $(SDK)/usr/lib
CC = gcc
CFLAGS = -dynamic -DNDEBUG -arch x86_64 $(INCLUDES)
#CFLAGS = -dynamic -g -Wall -arch x86_64 $(INCLUDES)
XCFLAGS = -O2
SHELL = /bin/sh
VPATH = ..:../bdd/utils:../bdd/src:../mu/src

muobj = bdd_interface.o bdd.o bdd_factor.o bdd_quant.o bdd_fns.o bdd_vfns.o \
        appl.o mu_interface.o mu.o

utilobj = double.o list.o hash.o alloc.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : mu.dylib

mu.dylib : ${muobj} bdd_table.o mu_table.o ${utilobj}
	$(LD) $(LDFLAGS) -o mu.dylib ${muobj} ${utilobj} bdd_table.o mu_table.o -lm -lc

libutils.a : ${utilobj}
	ar r libutils.a ${utilobj}
	ranlib libutils.a

bdd_table.o : ../bdd_table.c
mu_table.o : ../mu_table.c

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
	rm -f *.o *.a *.dylib

