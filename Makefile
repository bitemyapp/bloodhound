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

haddock:
	cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/bloodhound/docs' --contents-location='http://hackage.haskell.org/package/bloodhound'

# cp -R ./dist/doc/html/bloodhound/ bloodhound-0.1.0.1-docs
# tar cvzf --format=ustar -f bloodhound-0.1.0.1-docs.tar.gz bloodhound-0.1.0.1-docs
# curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary '@bloodhound-0.1.0.1-docs.tar.gz' 'https://bitemyapp:$PASSWORD@hackage.haskell.org/package/bloodhound-0.1.0.1/docs'
