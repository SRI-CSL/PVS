# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for both leopard and tiger
MACOSX_DEPLOYMENT_TARGET=11
CC=clang
# CFLAGS=-dynamic
CFLAGS=-g -O2 -Wall -pedantic -std=gnu99 -mtune=native -mcpu=apple-a14
LD=ld
SDK=$(shell xcrun --show-sdk-path)
LDFLAGS = -dylib -flat_namespace -undefined suppress -arch arm64 -platform_version macos 11.0.0 12.0 -L $(SDK)/usr/lib
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
