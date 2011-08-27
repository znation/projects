GLIBINCLUDES = -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
GLIBPATH = -lglib-2.0 -lintl -liconv -lgmp -lmpfr
GCCOPTS = -Werror -Wall -pedantic -std=c99 -O2 -march=pentium4 -g -pg $(GLIBINCLUDES) 
GCC = gcc $(GCCOPTS)

PROBLEM = Problem062
PROBLEMSRC = $(PROBLEM).c
PROBLEMDEP = Answer.h
PROBLEMOBJ = $(PROBLEM).o
OBJECTS = $(PROBLEMOBJ) Solver.o Utility.o BoundedArray.o

all: tags Solver.exe

$(PROBLEMOBJ): $(PROBLEMSRC) $(PROBLEMDEP)
	$(GCC) -c $(PROBLEMSRC)

Solver.o: Solver.c
	$(GCC) -c Solver.c

Utility.o: Utility.c Utility.h
	$(GCC) -c Utility.c

BoundedArray.o: BoundedArray.c BoundedArray.h
	$(GCC) -c BoundedArray.c

Solver.exe: $(OBJECTS)
	$(GCC) -o Solver.exe $(OBJECTS) $(GLIBPATH)

tags: *.c *.h
	ctags -R
    
clean:
	rm -f tags *.exe *.o
