COMMON = Debugging.hs Encoding.hs Statistics.hs WaveFormatEx.hs Wave.hs
SYNTH = $(COMMON) Synth.hs
TESTING = $(COMMON) Testing.hs

all: Synth.exe Testing.exe

Synth.exe: $(SYNTH)
	ghc --make -O2 -prof -auto-all -Wall Synth.hs

Testing.exe: $(TESTING)
	ghc --make -O2 -prof -auto-all -Wall Testing.hs 
    
clean:
	rm -f Synth.exe Testing.exe *.hi *.o
