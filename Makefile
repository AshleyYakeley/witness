default: complete

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling

build: configure
	cabal build --ghc-options=-Werror

haddock: configure
	cabal haddock

install: build
	cabal install --user --enable-library-profiling --enable-executable-profiling

complete: haddock install

sdist: configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install complete test sdist
