GHCOPTS = --make -Wall -auto-all -O2

all: clean Solver.exe

Solver.exe: Solver.hs
	ghc $(GHCOPTS) Solver.hs

clean:
	rm -f Solver Solver.exe *.hi *.o
