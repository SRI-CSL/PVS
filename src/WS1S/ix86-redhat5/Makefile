BDD = ../mona/BDD
DFA = ../mona/DFA
UTILS = ../mona/Mem
INCLUDES = -I$(BDD) -I$(DFA) -I$(UTILS)
LD = ld
LDFLAGS = -Bsymbolic -shared -L./
CC = gcc
CFLAGS += -D_POSIX_SOURCE -DSYSV $(INCLUDES)
XCFLAGS = -O
SHELL = /bin/sh
VPATH = ..:../mona/BDD:../mona/DFA:../mona/Mem

obj  = analyze.o prefix.o product.o \
       quotient.o basic.o external.o \
       makebasic.o minimize.o printdfa.o \
       project.o dfa.o \
       bdd.o bdd_double.o bdd_external.o \
       bdd_manager.o hash.o bdd_dump.o \
       bdd_trace.o bdd_cache.o \
       dlmalloc.o mem.o \
       ws1s_extended_interface.o 

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

ws1s_extended_interface.o : ../ws1s_extended_interface.c
	$(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : ws1s.so

ws1s.so : ${obj}
	$(LD) ../ws1s-ld-table $(LDFLAGS) -o ws1s.so ${obj}

bdd.o: bdd.c bdd.h bdd_internal.h
bdd_double.o: bdd_double.c bdd.h bdd_internal.h
bdd_external.o: bdd_external.c bdd_external.h mem.h
bdd_manager.o: bdd_manager.c bdd.h bdd_internal.h
hash.o: hash.c mem.h hash.h
bdd_dump.o: bdd_dump.c bdd_dump.h
bdd_trace.o: bdd_trace.c bdd.h bdd_internal.h
bdd_cache.o: bdd_cache.c bdd.h bdd_internal.h

analyze.o: analyze.c dfa.h mem.h
prefix.o: prefix.c dfa.h mem.h
product.o: product.c dfa.h bdd.h hash.h mem.h
quotient.o: quotient.c dfa.h hash.h mem.h
basic.o: basic.c dfa.h mem.h
external.o: external.c dfa.h bdd_external.h mem.h
makebasic.o: makebasic.c dfa.h bdd_internal.h
minimize.o: minimize.c dfa.h hash.h mem.h
printdfa.o: printdfa.c dfa.h mem.h
project.o: project.c dfa.h hash.h mem.h
dfa.o: dfa.c dfa.h bdd.h hash.h mem.h

dlmalloc.o: dlmalloc.c dlmalloc.h
mem.o: mem.c dlmalloc.h

clean : 
	rm -f *.o *.a *.so
