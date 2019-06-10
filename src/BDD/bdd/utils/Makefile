# Makefile for utilities library
# includes: hash table manipulation
#           list processing
#           Double number package
# Copyright (c) 1995-1997 G. Janssen

# specify appropriate C compiler flags here:
# include -DDO_CHECKS to incorporate range checks in bitvector routines
#CFLAGS= -Aa -D_POSIX_SOURCE
#CFLAGS= -D_POSIX_SOURCE
XCFLAGS= -O

obj=	double.o list.o hash.o alloc.o

utils : libutils.a

libutils.a : ${obj}
	ar r libutils.a ${obj}
	ranlib libutils.a

.c.o : ; $(CC) $(XCFLAGS) ${CFLAGS} -c $*.c

graph_build.o : graph_build.c graph.h graph_remap.h graph_build.h unitypes.h
graph.o : graph.c graph.h graph_remap.h graph_build.h unitypes.h

double.o : double.c double.h
list.o : list.c list.h alloc.h
hash.o : hash.c hash.h alloc.h
alloc.o : alloc.c alloc.h

dag.o : dag.c dag.h dag_customize.h general.h
tree.o : tree.c tree.h tree_customize.h general.h

install: utils
	cp *.h /usr/es/include/utils
	cp *.a /usr/es/lib

install_akebono: utils
	cp *.h /opt/es/include/utils
	cp *.a /opt/es/lib

clean : 
	rm -f *.o *.a .pure

what :
	# make       - makes the archive libutils.a
	# make utils - makes the archive libutils.a
	# make clean - cleans up objects and archive in this directory
