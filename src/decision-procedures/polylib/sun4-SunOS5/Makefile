INCLUDES = -I../
CC = gcc
CFLAGS = -fPIC -DCHECK_OVERFLOW
LD = $(CC)
LDFLAGS = -shared

VPATH = ../
obj = vector.o polyhedron.o foreign_funcs.o
.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) ${CFLAGS} -c $< -o $@

all: polylib.so

polylib.so: ${obj}
	$(LD) $(LDFLAGS) -o polylib.so ${obj}

vector.o : vector.c vector.h types.h
polyhedron.o: polyhedron.c vector.h polyhedron.h types.h
foreign_funcs.o: foreign_funcs.c vector.h polyhedron.h types.h

clean:
	-rm *.o
	-rm a.out
	-rm testlib
