LD = gcc
LDFLAGS = -shared -L./
CC=gcc
CFLAGS=-fPIC
WFLAGS=-Wall
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${WFLAGS} ${CFLAGS} -c $< -o $@

all : file_utils.so b64

file_utils.so: ${obj}
	$(LD) ../utils-ld-table $(LDFLAGS) -o file_utils.so ${obj} -lc

b64: ../b64.c
	$(CC) -o ./b64 ../b64.c

clean :
	rm -f *.o *.a *.so b64
