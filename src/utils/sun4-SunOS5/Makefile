# Makke sure ld --version returns >= 2.13.1
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

all : file_utils.so

file_utils.so: ${obj}
	$(LD) ../utils-ld-table $(LDFLAGS) -o file_utils.so ${obj}

clean :
	rm -f *.o *.a *.so
