COMMON = 
ANALYZE = analyze.hs
TESTING = testing.hs

all: analyze.exe testing.exe

analyze.exe: $(COMMON) $(ANALYZE)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(ANALYZE)

testing.exe: $(COMMON) $(TESTING)
	ghc --make -O2 -prof -auto-all -Wall -rtsopts $(TESTING)
    
clean:
	rm -f analyze.exe *.hi *.o
