BDD = ../mona-1.4/BDD
DFA = ../mona-1.4/DFA
UTILS = ../mona-1.4/Mem
MACOSX_DEPLOYMENT_TARGET=10.11
CC = clang
INCLUDES = -I$(BDD) -I$(DFA) -I$(UTILS)
CFLAGS = -g -O2 -Wall -pedantic -std=gnu99 -mtune=native -mcpu=apple-a14  $(INCLUDES)
XCFLAGS = -O2
# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for older versions of MacOS X
#
# NOTE: with my version of Xcode (on MacOS 10.11), ld does not seem able to parse
# the option properly. If we give -macosx_version_min 10.9, it prints this warning;
#  ld: warning: -macosx_version_min not specified, assuming 10.9
# If we give -macosx_version_min and omit the version number, it complains that a version
# number is required:
#  ld: -macosx_version_min argument missing
# So there doesn't seem to be any choice.
# 
LD=ld
LDFLAGS = -dylib -flat_namespace -undefined suppress -arch arm64 -platform_version macos 11.0.0 12.0  -L /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib
SHELL = /bin/sh
VPATH = ..:../mona-1.4/BDD:../mona-1.4/DFA:../mona-1.4/Mem

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

all : ws1s.dylib

ws1s_extended_interface.o : ../ws1s_extended_interface.c
	$(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

ws1s.dylib : ${obj} ws1s_table.o
	$(LD) $(LDFLAGS) -o ws1s.dylib ${obj} ws1s_table.o -lc

# ws1s_table.c : ws1s-ld-table
# 	awk '/^ws1s___/ { print "extern int " $3 " (int);\nint (*" $1 ")(int) = " $3 ";\n\n" }' ../ws1s-ld-table > ws1s_table.c

ws1s_table.o: ../ws1s_table.c

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
	rm -f *.o *.a *.dylib
