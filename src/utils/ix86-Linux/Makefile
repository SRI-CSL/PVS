LD = ld
LDFLAGS = -shared -L./
CC=gcc
CFLAGS=-fPIC
WFLAGS=-Wall
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${WFLAGS} ${CFLAGS} -c $< -o $@

all : file_utils.so

file_utils.so: ${obj}
	$(LD) ../utils-ld-table $(LDFLAGS) -o file_utils.so ${obj} -lc

clean :
	rm -f *.o *.a *.so
