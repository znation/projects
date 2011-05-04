COMMON = Quote.hs
ANALYZE = Analyze.hs
TESTING = Testing.hs

all: Analyze.exe Testing.exe

analyze.exe: $(COMMON) $(ANALYZE)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(ANALYZE)

testing.exe: $(COMMON) $(TESTING)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(TESTING)
    
clean:
	rm -f Analyze.exe Testing.exe *.hi *.o
