LD=gcc
LDFLAGS = -dynamiclib -L./
CC=cc
CFLAGS=-dynamic
WFLAGS=
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : file_utils.dylib b64

file_utils.dylib: ${obj}
	$(LD) $(LDFLAGS) -o file_utils.dylib ${obj} -lc

b64: ../b64.c
	$(CC) -o ./b64 ../b64.c

clean :
	rm -f *.o *.a *.dylib
