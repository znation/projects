Synth.exe:
	ghc --make Encoding.hs WaveFormatEx.hs Wave.hs Synth.hs
	rm -f *.hi *.o

clean:
	rm -f Synth.exe *.hi *.o

all: clean Synth.exe

