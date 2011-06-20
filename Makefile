GHCOPTS = --make -Wall -auto-all -O2 -prof -caf-all

all: clean Solver.exe

Solver.exe: Solver.hs
	ghc $(GHCOPTS) Solver.hs

clean:
	rm -f Solver Solver.exe *.hi *.o
