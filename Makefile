GHC = ghc

rb-cc: Main.hs
	$(GHC)  --make  -o rb-cc Main.hs

test: rb-cc
	./test.sh

clean:
	rm -f rb-cc *.o *~ tmp*

.PHONY: test clean
