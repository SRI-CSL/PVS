include ../config

SRC	= presburger_transduction.c presburger_analysis.c \
	  gta_example.c bdd_example.c
OBJ	= $(SRC:.c=.o)

MEM_OBJ = ../Mem/mem.a
BDD_OBJ = ../BDD/bdd.a
DFA_OBJ	= ../DFA/dfa.a
GTA_OBJ = ../GTA/gta.a

INCLUDES = -I../Mem -I../DFA -I../BDD -I../GTA

all:	FORCE presburger gta_example bdd_example

presburger: presburger_transduction presburger_analysis

presburger_%: presburger_%.c $(BDD_OBJ) $(DFA_OBJ) $(MEM_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(DFA_OBJ) $(BDD_OBJ) $(MEM_OBJ) -o $@

gta_example: gta_example.c $(BDD_OBJ) $(GTA_OBJ) $(MEM_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(GTA_OBJ) $(BDD_OBJ) $(MEM_OBJ) -o $@

bdd_example: bdd_example.c $(BDD_OBJ) $(MEM_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) $< $(BDD_OBJ) $(MEM_OBJ) -o $@

clean:	FORCE
	-/bin/rm -f $(OBJ)

depend:	FORCE
	$(CXX) -MM $(INCLUDES) $(SRC) > dependencies

include dependencies

FORCE:
