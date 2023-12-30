GHC= ghc
SRCS=$(wildcard *.hs)
OBJS=$(SRCS:=.hs=.o)

rb-cc: $(SRCS)
	$(GHC) --make  -o rb-cc $(SRCS)

test: rb-cc
	./test.sh

clean:
	rm -f rb-cc *.o *~ tmp*

.PHONY: test clean
