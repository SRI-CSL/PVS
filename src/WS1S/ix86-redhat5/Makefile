BDD = ../mona/BDD
DFA = ../mona/DFA
UTILS = ../mona/BDD/long_mem
INCLUDES = -I$(BDD) -I$(DFA) -I$(UTILS)
LD = $(CC)
LDFLAGS = -shared -L./
CC = gcc
CFLAGS += -D_POSIX_SOURCE -DSYSV $(INCLUDES)
XCFLAGS = -O
SHELL = /bin/sh
VPATH = ..:../mona/BDD:../mona/DFA:../mona/BDD/long_mem

obj  = analyze.o prefix.o product.o \
       quotient.o basic.o external.o \
       makebasic.o minimize.o printdfa.o \
       project.o dfa.o \
       bdd.o bdd_double.o bdd_external.o \
       bdd_manager.o hash.o bdd_dump.o \
       bdd_trace.o bdd_cache.o \
       memblock.o memrec.o \
       ws1s_extended_interface.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : ws1s.so

ws1s.so : ${obj}
	$(LD) ../ws1s-ld-table $(LDFLAGS) -o ws1s.so ${obj}

bdd.o: bdd.c bdd.h bdd_internal.h memuser.h
bdd_double.o: bdd_double.c bdd.h bdd_internal.h memuser.h
bdd_external.o: bdd_external.c bdd_external.h bdd.h
bdd_manager.o: bdd_manager.c bdd.h bdd_internal.h memuser.h
hash.o: hash.c memuser.h hash.h
bdd_dump.o: bdd_dump.c bdd_dump.h bdd.h
bdd_trace.o: bdd_trace.c bdd.h bdd_internal.h memuser.h
bdd_cache.o: bdd_cache.c bdd.h bdd_internal.h memuser.h

analyze.o: analyze.c dfa.h bdd.h
prefix.o: prefix.c dfa.h bdd.h
product.o: product.c dfa.h bdd.h hash.h memuser.h
quotient.o: quotient.c dfa.h bdd.h hash.h memuser.h
basic.o: basic.c dfa.h bdd.h
external.o: external.c dfa.h bdd.h bdd_external.h
makebasic.o: makebasic.c dfa.h bdd.h bdd_internal.h memuser.h
minimize.o: minimize.c dfa.h bdd.h hash.h memuser.h
printdfa.o: printdfa.c dfa.h bdd.h
project.o: project.c dfa.h bdd.h hash.h memuser.h
dfa.o: dfa.c dfa.h bdd.h hash.h memuser.h

ws1s_extended_interface.o : ws1s_extended_interface.c dfa.h

clean : 
	rm -f *.o *.a *.so
