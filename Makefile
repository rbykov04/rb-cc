GHC= ghc
SRCS=$(wildcard *.hs)
OBJS=$(SRCS:=.hs=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

rb-cc: $(SRCS)
	cabal build


test/%.exe: rb-cc test/%.c
	$(CC) -o- -E -P -C test/$*.c | cabal run rb-cc -- -o test/$*.s -
	$(CC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh


clean:
	rm -rf rb-cc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean rb-cc
