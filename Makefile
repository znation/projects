GHCOPTS = --make -Wall -auto-all -O2 -prof -caf-all
 
all: Solver InputSolver
 
Solver: *.hs
	ghc $(GHCOPTS) Solver.hs
 
InputSolver: *.hs
	ghc $(GHCOPTS) InputSolver.hs
	 
clean:
	rm -f Solver InputSolver *.hi *.o

