.PHONY : build build-validate ghci test test-rerun test-ghci ghcid ghcid-validate \
         weeder hlint hlint-watch mod-build

.DEFAULT_GOAL = help

ghc_perf_options = --ghc-options '+RTS -A128M -RTS'
build = build $(ghc_perf_options)
ghci = repl $(ghc_perf_options)
test = test $(ghc_perf_options)

## run build
build:
	cabal $(build)

## build with validation options (Wall, Werror)
build-validate:
	cabal build --disable-optimization --ghc-options '-Wall -Werror +RTS -A128M -RTS'

## run ghci
ghci:
	cabal $(ghci)

## run tests
test: echo-warn
	cabal $(test)

## run tests with forced re-run via "-r"
test-rerun: echo-warn
	cabal $(test) --test-options='-r --failure-report test-failure-report'

## run ghci with test stanza
test-ghci:
	cabal $(ghci) bloodhound:test:bloodhound-tests

## run ghcid
ghcid:
	ghcid

## run weeder
weeder:
	weeder . --build

## run hlint
hlint:
	hlint .

## hlint watch with `sos`
hlint-watch:
	sos src/ -c "hlint ." -p "src/(.*)\.hs"

# mod-build:
# 	stack build --ghc-options '+RTS -A128M -RTS'

echo-warn:
	@echo "Make certain you have an elasticsearch instance on localhost:9200 !"

# Create ES instance

## Run test environment
compose:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml up

## Run test environment in detach mode
compose-detach-up:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml up -d

## Close test environment if run on detach mode
compose-detach-down:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml down

## build the docker compose images
compose-build:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml build

## Spawn bash shell in test container
compose-shell:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml exec elasticsearch1 bash

help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
