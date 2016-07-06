build:
	stack build

test:
	echo "Make certain you have an elasticsearch instance on localhost:9200 !"
	stack test

ghci:
	stack ghci

upload:
	stack upload --no-signature .
