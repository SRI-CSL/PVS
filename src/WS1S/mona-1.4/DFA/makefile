include ../config

SRC	= dfa.c basic.c printdfa.c makebasic.c external.c product.c \
	  project.c minimize.c quotient.c prefix.c analyze.c

OBJ	= $(SRC:.c=.o)

INCLUDES = -I../BDD

dfa.a:	$(OBJ)
	$(AR) -r $@ ${OBJ}

%.o:	%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

depend:	FORCE
	$(CXX) -MM $(INCLUDES) $(SRC) > dependencies

clean:	FORCE
	-/bin/rm -f $(OBJ) dfa.a

include dependencies

FORCE:
