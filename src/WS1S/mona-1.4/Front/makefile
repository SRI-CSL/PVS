include ../config

SRC 	= ast.cpp code.cpp offsets.cpp st_dfa.cpp predlib.cpp \
	  signature.cpp symboltable.cpp untyped.cpp printline.cpp \
	  timer.cpp st_gta.cpp mona.cpp ident.cpp lib.cpp reduce.cpp \
	  freevars.cpp astdump.cpp codetable.cpp codedump.cpp \
	  makeguide.cpp codesubst.cpp
OBJ	= $(SRC:.cpp=.o) scanner.o parser.o

INCLUDES = -I../BDD
CFLAGS	+= -pedantic

front.a: $(OBJ)
	$(AR) -r $@ ${OBJ}

mona.o:	mona.cpp
	$(CXX) $(CFLAGS) -DVERSION=\"$(VERSION)\" $(INCLUDES) -c mona.cpp

scanner.cpp: scanner.l parser.h 
	flex -l -oscanner.cpp scanner.l

scanner.o: scanner.cpp
	$(CXX) $(CFLAGS) -Wno-unused $(INCLUDES) -c scanner.cpp

parser.cpp: parser.y
	bison -y -d -o parser.c parser.y
	mv parser.c parser.cpp

parser.h: parser.cpp

%.o:	%.cpp
	$(CXX) $(CFLAGS) $(INCLUDES) -c $<

depend:	FORCE scanner.cpp parser.cpp
	$(CXX) -MM $(SRC) scanner.cpp parser.cpp $(INCLUDES) > dependencies

clean:	FORCE
	-/bin/rm -f $(OBJ) front.a scanner.cpp parser.h parser.cpp

wc:	FORCE
	wc -l \
	`find . \
	\( -name '*.h' -o \
	-name '*.cpp' -o \
	-name '*.c' -o \
	-name '*.l' -o \
	-name '*.y' -o \
	-name makefile -o \
	-name config \) -a \
	! -name 'scanner.cpp' -a \
	! -name 'parser.cpp' -a \
	! -name 'parser.h' `

include dependencies

FORCE:
