include	../config

SRC =	bdd.c bdd_manager.c bdd_cache.c bdd_double.c bdd_trace.c \
	hash.c bdd_external.c bdd_dump.c

OBJ	= ${SRC:.c=.o}

bdd.a:	${OBJ}
	$(AR) -r $@ ${OBJ}

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

example: ${OBJ} example.o
	$(CC) $(CFLAGS) $(INCLUDES) ${OBJ} example.o -o example

depend: FORCE
	$(CC) -MM $(SRC) > dependencies

clean:	FORCE
	-/bin/rm -f $(OBJ) bdd.a

include dependencies

FORCE:
