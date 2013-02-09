all: build

.PHONY: all build dist install clean doc p ghci

build: dist/setup-config
	rm -rf _site _cache
	cabal-dev build

dist:
	cabal-dev sdist

install: build
	cabal install

clean:
	cabal-dev clean
	rm -rf cabal-dev/

dist/setup-config: wordwang.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure --enable-tests || cabal-dev install --enable-tests

doc: build
	cabal-dev haddock

p:
	permamake.sh *.hs \
	             *.cabal \
	             Makefile

run: build
	dist/build/wordwang/wordwang

ghci: build
	cabal-dev ghci
