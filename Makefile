GCCOPTS = -Werror -Wall -std=c99 -march=pentium4 -O3 -g -pg
GCC = gcc $(GCCOPTS)

PROBLEM = Problem037
PROBLEMSRC = $(PROBLEM).c
PROBLEMOBJ = $(PROBLEM).o
OBJECTS = $(PROBLEMOBJ) Solver.o Utility.o DynArray.o

all: Solver.exe

$(PROBLEMOBJ): $(PROBLEMSRC)
	$(GCC) -c $(PROBLEMSRC)

Solver.o: Solver.c
	$(GCC) -c Solver.c

Utility.o: Utility.c
	$(GCC) -c Utility.c

DynArray.o: DynArray.c
	$(GCC) -c DynArray.c

Solver.exe: $(OBJECTS)
	$(GCC) -o Solver.exe $(OBJECTS)
    
clean:
	rm -f *.exe *.o
