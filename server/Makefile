all: build test

.PHONY: all build dist install clean doc p test ghci

build: dist/setup-config
	cabal-dev build

dist:
	cabal-dev sdist

install: build
	cabal install

clean:
	cabal-dev clean
	rm -rf cabal-dev/ log/

dist/setup-config: wordwang.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure --enable-tests || cabal-dev install --enable-tests

doc: build
	cabal-dev haddock

test: build
	cabal-dev test

p:
	permamake.sh *.hs \
	             Test/*.hs \
	             *.cabal \
	             Makefile

run: build
	mkdir -p log/
	dist/build/wordwang/wordwang

ghci: build
	cabal-dev ghci
