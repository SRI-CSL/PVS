include ../config

SRC	= dlmalloc.c mem.c

OBJ	= $(SRC:.c=.o)

mem.a:  $(OBJ)
	$(AR) -r $@ ${OBJ}

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

depend: FORCE
	$(CC) -MM $(SRC) > dependencies

clean:  FORCE
	-/bin/rm -f $(OBJ) mem.a

include dependencies

FORCE:
