include ../config

SRC	= presburger_transduction.c presburger_analysis.c \
	  gta_example.c bdd_example.c
OBJ	= $(SRC:.c=.o)

BDD_OBJ = ../BDD/bdd.o ../BDD/bdd_cache.o ../BDD/bdd_double.o \
	  ../BDD/bdd_manager.o ../BDD/bdd_trace.o ../BDD/hash.o \
	  ../BDD/long_mem/memrec.o ../BDD/long_mem/memblock.o \
	  ../BDD/bdd_external.o ../BDD/bdd_dump.o 

DFA_OBJ	= ../DFA/dfa.o ../DFA/basic.o ../DFA/analyze.o \
	  ../DFA/printdfa.o ../DFA/makebasic.o ../DFA/external.o \
	  ../DFA/project.o ../DFA/quotient.o ../DFA/product.o \
	  ../DFA/minimize.o

GTA_OBJ = ../GTA/external.o ../GTA/analyze_acceptance.o ../GTA/gta.o

INCLUDES = -I../DFA -I../BDD -I../GTA

all:	presburger gta_example bdd_example

presburger: presburger_transduction presburger_analysis

presburger_%: presburger_%.c $(BDD_OBJ) $(DFA_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(BDD_OBJ) $(DFA_OBJ) -o $@

gta_example: gta_example.c $(BDD_OBJ) $(GTA_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(BDD_OBJ) $(GTA_OBJ) -o $@

bdd_example: bdd_example.c $(BDD_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(BDD_OBJ) -o $@

clean:	FORCE
	-/bin/rm -f $(OBJ)

depend:	FORCE
	$(CXX) -MM $(INCLUDES) $(SRC) > dependencies

include dependencies

FORCE:
