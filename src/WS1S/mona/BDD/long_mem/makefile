include ../../config

SRC	= memblock.c \
	  memrec.c 

OBJ	= $(SRC:.c=.o)

CFLAGS	+= -D__STDC__

%.o:	%.c
	$(CC) $(CFLAGS) -c $<

libmem.a: $(OBJ)

depend: ALWAYS
	$(CC) -MM $(SRC) > dependencies

clean:  ALWAYS
	-/bin/rm -f $(OBJ)

include dependencies

ALWAYS:
