GHCOPTS = --make -Wall -auto-all -O2 -prof -caf-all

all: Solver InputSolver

Solver: Solver.hs
	ghc $(GHCOPTS) Solver.hs

InputSolver: InputSolver.hs
	ghc $(GHCOPTS) InputSolver.hs
    
clean:
	rm -f Solver InputSolver *.hi *.o
