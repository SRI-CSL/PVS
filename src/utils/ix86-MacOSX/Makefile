# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for both leopard and tiger
MACOSX_DEPLOYMENT_TARGET=10.2
LD=ld
LDFLAGS = -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -L./ -weak_library /usr/lib/libSystem.B.dylib
CC=cc
CFLAGS=-dynamic
WFLAGS=
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : file_utils.dylib b64

file_utils.dylib: ${obj} utils_table.o
	$(LD) $(LDFLAGS) -o file_utils.dylib ${obj} utils_table.o -lc

utils_table.o : ../utils_table.c

b64: ../b64.c
	$(CC) -o ./b64 ../b64.c

clean :
	rm -f *.o *.a *.dylib
