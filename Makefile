default: build

# Building

clean:
	runhaskell Setup.hs clean

configure:
	runhaskell Setup.hs configure --enable-library-profiling

build: configure
	runhaskell Setup.hs build

haddock: configure
	runhaskell Setup.hs haddock

install: build haddock
	sudo runhaskell Setup.hs install

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test

