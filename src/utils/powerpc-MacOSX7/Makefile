LD=ld
LDFLAGS = -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -L./
CC=cc
CFLAGS=-dynamic
WFLAGS=
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : file_utils.dylib

file_utils.dylib: ${obj} utils_table.o
	$(LD) $(LDFLAGS) -o file_utils.dylib ${obj} utils_table.o -lc

clean :
	rm -f *.o *.a *.so
