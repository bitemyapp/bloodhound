
test:
	cabal install --enable-tests

build:
	cabal build

test-repl:
	cabal repl tests

repl:
	cabal repl
