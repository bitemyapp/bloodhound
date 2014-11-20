install-with-test-support:
	cabal install --enable-tests
	echo "Make certain you have an elasticsearch instance on localhost:9200 !"
	cabal test

test:
	echo "Make certain you have an elasticsearch instance on localhost:9200 !"
	cabal clean
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
