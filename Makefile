# haskell compiler
HC=ghc
HLIB=parsec
CC=gcc

all: src/*.hs lib
	$(HC) -o coyote --make -package $(HLIB) src/Main.hs -isrc
lib:
	$(CC) -c -o stdlib/stdlib.o stdlib/stdlib.c -Wall -pedantic -ansi -Wextra

clean:
	rm src/*.hi
	rm src/*.o
	rm stdlib/*.o
	rm coyote
	@echo "done"

