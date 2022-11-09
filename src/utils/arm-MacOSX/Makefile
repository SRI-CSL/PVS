# MACOSX_DEPLOYMENT_TARGET and -weak_library are needed so that dylibs work
# for both leopard and tiger
MACOSX_DEPLOYMENT_TARGET=11
CC=clang
# CFLAGS=-dynamic
CFLAGS=-g -O2 -Wall -pedantic -std=gnu99 -mtune=native -mcpu=apple-a14
LD=/opt/homebrew/Cellar/llvm/14.0.6_1/bin/ld64.lld
LDFLAGS = -dylib -flat_namespace -undefined suppress -arch arm64 -platform_version macos 11.0.0 12.0
WFLAGS=
VPATH=..

obj=file_utils.o

.SUFFIXES:
.SUFFIXES: .c .o
.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $< -o $@

all : file_utils.dylib b64

file_utils.dylib: ${obj} utils_table.o
	$(LD) $(LDFLAGS) -o file_utils.dylib ${obj} utils_table.o

utils_table.o : ../utils_table.c

b64: ../b64.c
	$(CC) -o ./b64 ../b64.c

clean :
	rm -f *.o *.a *.dylib
