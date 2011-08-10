GLIBINCLUDES = -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
GLIBPATH = -lglib-2.0 -lintl -liconv
GCCOPTS = -Werror -Wall -std=c99 -march=pentium4 -O2 -g -pg $(GLIBINCLUDES) 
GCC = gcc $(GCCOPTS)

PROBLEM = Problem057
PROBLEMSRC = $(PROBLEM).c
PROBLEMDEP = Answer.h
PROBLEMOBJ = $(PROBLEM).o
OBJECTS = $(PROBLEMOBJ) Solver.o Utility.o giants.o 

all: tags Solver.exe

$(PROBLEMOBJ): $(PROBLEMSRC) $(PROBLEMDEP)
	$(GCC) -c $(PROBLEMSRC)

Solver.o: Solver.c
	$(GCC) -c Solver.c

Utility.o: Utility.c Utility.h
	$(GCC) -c Utility.c

giants.o: giants.c giants.h
	gcc -w -c giants.c

Solver.exe: $(OBJECTS)
	$(GCC) -o Solver.exe $(OBJECTS) $(GLIBPATH)

tags: *.c *.h
	ctags -R
    
clean:
	rm -f tags *.exe *.o
