GHC= ghc
SRCS=$(wildcard *.hs)
OBJS=$(SRCS:=.hs=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

rb-cc: $(SRCS)
	$(GHC) --make  -o rb-cc $(SRCS)

test/%.exe: rb-cc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./rb-cc -o test/$*.s -
	$(CC) -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done

clean:
	rm -rf rb-cc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean
