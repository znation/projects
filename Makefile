GLIBINCLUDES = -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
GLIBPATH = -lglib-2.0 -lintl -liconv
GCCOPTS = -Werror -Wall -std=c99 -march=pentium4 -O3 -g -pg $(GLIBINCLUDES) 
GCC = gcc $(GCCOPTS)

PROBLEM = Problem043
PROBLEMSRC = $(PROBLEM).c
PROBLEMOBJ = $(PROBLEM).o
OBJECTS = $(PROBLEMOBJ) Solver.o Utility.o 

all: Solver.exe

$(PROBLEMOBJ): $(PROBLEMSRC)
	$(GCC) -c $(PROBLEMSRC)

Solver.o: Solver.c
	$(GCC) -c Solver.c

Utility.o: Utility.c
	$(GCC) -c Utility.c

Solver.exe: $(OBJECTS)
	$(GCC) -o Solver.exe $(OBJECTS) $(GLIBPATH)
    
clean:
	rm -f *.exe *.o
