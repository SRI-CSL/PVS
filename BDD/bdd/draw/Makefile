# Makefile for Reduced Ordered Boolean Decision Diagram Manipulation
# includes: lexical analyser + parser
# Copyright (c) 1990-1996 G. Janssen

UTILS=../utils
SRC=../src
INCLUDES   = -I. -I${SRC} -I$(UTILS) -I/usr/include/X11R5

ARS = ${SRC}/libbdd2.a $(UTILS)/libutils.a

# specify appropriate C compiler flags here:
#CFLAGS= -O -D_POSIX_SOURCE $(INCLUDES)
CFLAGS= -O -Aa -D_POSIX_SOURCE $(INCLUDES)
LDFLAGS=-s

objbdd= ${SRC}/vfns.o ${SRC}/bdd_factor.o ${SRC}/bdd_fns.o ${SRC}/bdd_quant.o \
        ${SRC}/bdd.o
objX=	main.o plot.o draw_X.o
objA=	main.o plot.o draw_ascii.o
objP=	main.o plot.o draw_ps.o
#objB=	main.o plot.o draw_backdraw.o run_child.o
objB=	main.o plot.o draw_backdraw.o

bdd2X : ${objX}
	$(CC) $(LDFLAGS) -o bdd2X ${objX} $(ARS) -ll -lm -L/usr/lib/X11R5 -lX11
#	$(CC) -o bdd2X ${objX} $(ARS) -ll

bdd2ascii : ${objA}
	$(CC) $(LDFLAGS) -o bdd2ascii ${objA} $(ARS) -ll

bdd2ps : ${objP}
	$(CC) $(LDFLAGS) -o bdd2ps ${objP} $(ARS) -ll

bdd2backdraw : ${objB}
	$(CC) $(LDFLAGS) -o bdd2backdraw ${objB} $(ARS) -ll

.c.o : ; $(CC) ${XCFLAGS} ${CFLAGS} -c $*.c

plot.o : plot.c plot.h
draw_ascii.o : draw_ascii.c plot.h
draw_X.o : draw_X.c plot.h bdd_icon.h
draw_ps.o : draw_ps.c plot.h
#draw_backdraw.o : draw_backdraw.c plot.h run_child.h
draw_backdraw.o : draw_backdraw.c plot.h
main.o : main.c plot.h
#run_child.o : run_child.c run_child.h

Xlib :  libbddX.a

libbddX.a: plot.o draw_X.o
	ar r libbddX.a plot.o draw_X.o ${objbdd}
	ranlib libbddX.a

Alib :  libbddA.a

libbddA.a : draw_ascii.o
	ar r libbddA.a plot.o draw_ascii.o ${objbdd}
	ranlib libbddA.a

Plib :  libbddP.a

libbddP.a : plot.o draw_ps.o
	ar r libbddP.a plot.o draw_ps.o ${objbdd}
	ranlib libbddP.a

Blib :  libbddB.a

libbddB.a : plot.o draw_backdraw.o
	ar r libbddB.a plot.o draw_backdraw.o ${objbdd}
	ranlib libbddB.a

clean : 
	rm -f *.o *.a

all:	bdd2X bdd2backdraw bdd2ascii bdd2ps Xlib Alib Plib Blib

install: all
	cp *.a /usr/es/lib
	cp *.h /usr/es/include/bdd
	mv bdd2* /usr/es/bin

install_akebono: all
	cp *.a /opt/es/lib
	cp *.h /opt/es/include/bdd
	mv bdd2* /opt/es/bin

what :
	# make             - makes the bdd2X program
	# make bdd2X       - makes the bdd2X program
	# make bdd2ps      - makes the bdd2ps program
	# make bdd2ascii   - makes the bdd2ascii program
	# make bdd2backdraw - makes the bdd2backdraw program
	# make all         - makes the bdd2* programs
	# make Xlib        - creates the archive libbddX.a
	# make Plib        - creates the archive libbddP.a
	# make Alib        - creates the archive libbddA.a
	# make Blib        - creates the archive libbddB.a
	# make clean       - cleans up object and archives in this directory
