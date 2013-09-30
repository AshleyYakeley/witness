default: install

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling

build: configure
	cabal build --ghc-options=-Werror

haddock: configure
	cabal haddock

copy: build test haddock
	cabal copy

install:
	cabal install --user --ghc-options=-Werror --enable-library-profiling --enable-executable-profiling --force-reinstalls

sdist: clean configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock copy install sdist
