COMMON = Quote.hs Seed.hs TradeState.hs Trader.hs
ANALYZE = Analyze.hs
TESTING = Testing.hs

all: Analyze.exe Testing.exe

Analyze.exe: $(COMMON) $(ANALYZE)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(ANALYZE)

Testing.exe: $(COMMON) $(TESTING)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(TESTING)
    
clean:
	rm -f Analyze.exe Testing.exe *.hi *.o
