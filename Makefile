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
	cabal haddock --hoogle --hyperlink-source --html-location='http://hackage.haskell.org/package/bloodhound/docs' --contents-location='http://hackage.haskell.org/package/bloodhound'

dist:
	cabal sdist

# export BHVER=0.3.0.0
# upload:
# 	cabal upload dist/bloodhound-$BHVER.tar.gz
# cp -R ./dist/doc/html/bloodhound/ bloodhound-$BHVER-docs
# tar cvz --format=ustar -f bloodhound-$BHVER-docs.tar.gz bloodhound-$BHVER-docs
# curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@bloodhound-$BHVER-docs.tar.gz" "https://bitemyapp:$PASSWORD@hackage.haskell.org/package/bloodhound-$BHVER/docs"
