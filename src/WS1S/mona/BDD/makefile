include	../config

SRC =	bdd.c \
	bdd_manager.c \
	bdd_cache.c \
	bdd_double.c \
	bdd_trace.c \
	hash.c \
	bdd_external.c \
	bdd_dump.c

OBJ	= ${SRC:.c=.o} 

LONG_MEM_OBJ = long_mem/memblock.o long_mem/memrec.o 

all:	${OBJ} long

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

example: ${OBJ} example.o long
	$(CC) $(CFLAGS) $(INCLUDES) ${OBJ} \
	${LONG_MEM_OBJ} example.o -o example

long:	ALWAYS
	(cd long_mem; $(MAKE))

depend: ALWAYS
	(cd long_mem; $(MAKE) depend)
	$(CC) -MM $(SRC) > dependencies

clean:	ALWAYS
	(cd long_mem; $(MAKE) clean)
	-/bin/rm -f $(OBJ)

include dependencies

ALWAYS:

