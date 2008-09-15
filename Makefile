default: build

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling

build: configure
	cabal build --ghc-options="-Wall -Werror"

haddock: configure
	cabal haddock

install: build haddock
	cabal install --user --enable-library-profiling --enable-executable-profiling

sdist: configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test sdist
