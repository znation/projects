GCCOPTS = -Werror -Wall -std=c99 -march=pentium4 -O3 -o solver.exe

all: solver.exe

solver.exe: *.c
	gcc $(GCCOPTS) Solver.c
    
clean:
	rm -f *.exe *.o
