.PHONY: all haddock hpc install ghci tests cov test

all: haddock hpc install ghci test

haddock:
	stack haddock
	# .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/doc/html/moodle/doc-index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	stack install

ghci:
	stack ghci

tests:
	stack test :moodle-tests

cov:
	stack test :moodle-tests --coverage

test:
	stack test
