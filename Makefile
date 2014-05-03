
test:
	cabal test

build:
	cabal build

test-repl:
	cabal repl tests

repl:
	cabal repl

reset:
	rm -rf .cabal-sandbox
	cabal sandbox init

haddock:
	cabal haddock --hyperlink-source
