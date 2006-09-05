# Make sure ld --version returns >= 2.13.1
# LD = /homes/owre/sunos/bin/ld
LD=ld
LDFLAGS = -Bsymbolic -G -L./
CC=gcc
CFLAGS=-fPIC
WFLAGS=-Wall
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : file_utils.so b64

file_utils.so: ${obj}
	$(LD) ../utils-ld-table $(LDFLAGS) -o file_utils.so ${obj}

b64: ../b64.c
	$(CC) -o ./b64 ../b64.c

clean :
	rm -f b64 *.o *.a *.so
