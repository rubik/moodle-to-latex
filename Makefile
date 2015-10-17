.PHONY: all bench build clean configure haddock hpc install repl run test tests

all: install configure build haddock test hpc bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests --enable-coverage

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/moodle/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --enable-coverage --jobs \
		--only-dependencies --reorder-goals

repl:
	cabal repl lib:moodle

run:
	cabal run --jobs moodle

tests:
	cabal test tests --jobs --show-details=always --test-option=--color

test:
	cabal test --jobs
	cabal check
