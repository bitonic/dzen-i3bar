
all: DzenI3.hs Dzen.hs I3.hs
	ghc -O2 DzenI3.hs

clean:
	rm -f *.o *.hi
	rm -f DzenI3