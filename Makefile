COMMON = Generation.hs Portfolio.hs Quote.hs Seed.hs Trade.hs
ANALYZE = Analyze.hs
GETQUOTES = GetQuotes.hs
TESTING = Testing.hs
GHCOPTS = --make -Wall -prof -auto-all -rtsopts

all: Analyze.exe Testing.exe

Analyze.exe: $(COMMON) $(ANALYZE)
	ghc $(GHCOPTS) $(ANALYZE)

Testing.exe: $(COMMON) $(TESTING)
	ghc $(GHCOPTS) $(TESTING)

clean:
	rm -f Analyze.exe Testing.exe *.hi *.o
