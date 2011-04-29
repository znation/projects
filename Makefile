COMMON = Debugging.hs Encoding.hs Statistics.hs WaveFormatEx.hs Wave.hs

all: Synth.exe Testing.exe

Synth.exe: $(COMMON) Synth.hs
	ghc --make Synth.hs

Testing.exe: $(COMMON) Testing.hs
	ghc --make Testing.hs 

clean:
	rm -f Synth.exe Testing.exe *.hi *.o
