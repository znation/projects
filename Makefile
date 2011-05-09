COMMON = Quote.hs Seed.hs
ANALYZE = Analyze.hs
TESTING = Testing.hs

all: Analyze.exe Testing.exe

Analyze.exe: $(COMMON) $(ANALYZE)
	ghc --make -prof -auto-all -Wall -rtsopts $(ANALYZE)

Testing.exe: $(COMMON) $(TESTING)
	ghc --make -prof -auto-all -Wall -rtsopts $(TESTING)
    
clean:
	rm -f Analyze.exe Testing.exe *.hi *.o
