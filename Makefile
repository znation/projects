synth: clean
	ghc --make Encoding.hs WaveFormatEx.hs Wave.hs Synth.hs

test: clean
	ghc --make Encoding.hs WaveFormatEx.hs Wave.hs Testing.hs 

clean:
	rm -f Synth.exe Test.exe *.hi *.o

all: clean synth test
