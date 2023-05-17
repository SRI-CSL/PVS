# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for both leopard and tiger
MACOSX_DEPLOYMENT_TARGET=10.11
LD=ld
SDK=$(shell xcrun --show-sdk-path)
LDFLAGS = -bundle -flat_namespace -undefined suppress -L $(SDK)/usr/lib
CC=gcc
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

utils_table.o : ../utils_table.c

clean :
	rm -f *.o *.a *.dylib
