include	../config

SRC	= external.c analyze.c negation.c \
	  copy.c replace_indices.c printgta.c gta.c product.c \
	  pairhash.c project.c minimize.c subsets.c projset.c \
	  basic.c makebasic.c reachable.c dyn.c restrict.c \
	  analyze_acceptance.c types.c

OBJ	= ${SRC:.c=.o} 

INCLUDES = -I../BDD -I../BDD/long_mem
CFLAGS	+= -pedantic

gta.a:	${OBJ}
	$(AR) -r $@ ${OBJ}

%.o:	%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

depend:	FORCE
	$(CXX) -MM $(SRC) $(INCLUDES) > dependencies

clean:	FORCE
	-/bin/rm -f $(OBJ) gta.a

wc:	FORCE
	wc -l *.h *.c

include dependencies

FORCE: 
