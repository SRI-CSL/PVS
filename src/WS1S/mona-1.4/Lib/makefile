include ../config

SRC	= dfa2dot.c gta2dot.c dfalib.c gtalib.c
OBJ	= $(SRC:.c=.o)

CFLAGS	+= -pedantic

all:	FORCE $(OBJ) dfa2dot gta2dot

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

dfa2dot: dfa2dot.o dfalib.o
	$(CC) $(CFLAGS) $^ -o $@

gta2dot: gta2dot.o gtalib.o
	$(CC) $(CFLAGS) $^ -o $@

depend:	FORCE
	$(CC) -MM $(SRC) > dependencies

clean:	FORCE
	-/bin/rm -f $(OBJ)

include dependencies

FORCE:
