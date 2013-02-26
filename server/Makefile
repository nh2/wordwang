CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: build test

.PHONY: all build dist install clean doc p test ghci

build: dist/setup-config
	$(CABAL) build

dist:
	$(CABAL) sdist

install: build
	cabal install --force-reinstalls

clean:
	$(CABAL) clean
	rm -rf cabal-dev/ log/

dist/setup-config: wordwang.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure --enable-tests || $(CABAL) install --enable-tests

doc: build
	$(CABAL) haddock

test: build
	$(CABAL) test

p:
	permamake.sh $(shell find src/ -name '*.hs') \
	             $(shell find src/ -name '*.hs') \
	             wordwang.hs \
	             *.cabal \
	             Makefile

run: build
	mkdir -p log/
	dist/build/wordwang/wordwang

ghci: build
	cabal-dev ghci
