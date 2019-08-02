.PHONY : build build-validate ghci test test-rerun test-ghci ghcid ghcid-validate \
         weeder hlint hlint-watch mod-build

.DEFAULT_GOAL = help

stack = STACK_YAML='stack.yaml' stack
ghc_perf_options = --ghc-options '+RTS -A128M -RTS'
build = build $(ghc_perf_options)
ghci = ghci $(ghc_perf_options)
test = test $(ghc_perf_options)
stack-8.0 = STACK_YAML="stack-8.0.yaml" stack
stack-8.2 = STACK_YAML="stack-8.2.yaml" stack
stack-8.4 = STACK_YAML="stack-8.4.yaml" stack
stack-8.6 = STACK_YAML="stack-8.6.yaml" stack
elasticsearch_directory = elasticsearch

# stack build --ghc-options '+RTS -A128M -RTS'

## run build
build:
	$(stack) $(build)

## build with validation options (Wall, Werror)
build-validate:
	$(stack) build --fast --ghc-options '-Wall -Werror +RTS -A128M -RTS'

## run ghci
ghci:
	$(stack) $(ghci)

## run tests
test: echo-warn
	$(stack) $(test)

## run tests with forced re-run via "-r"
test-rerun: echo-warn
	$(stack) $(test) --test-arguments "-r"

## run ghci with test stanza
test-ghci:
	$(stack) $(ghci) bloodhound:test:bloodhound-tests

## run ghcid
ghcid:
	ghcid -c "$(stack) ghci bloodhound:lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is bloodhound:test:bloodhound-tests"

## run ghcid with validate options (Werror, etc.)
ghcid-validate:
	ghcid -c "$(stack) ghci bloodhound:lib --test --ghci-options='-Werror -fobject-code -fno-warn-unused-do-bind' --main-is bloodhound:test:bloodhound-tests"

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

## Test with GHC 8.0 and ES 5.x
test-8.0:
	STACK_YAML="stack-8.0.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500"

## Test with GHC 8.2 and ES 5.x
test-8.2:
	STACK_YAML="stack.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500"

## Build with the GHC 8.0 Stack YAML
build-8.0:
	$(stack-8.0) $(build)

## Build with the GHC 8.2 Stack YAML
build-8.2:
	$(stack-8.2) $(build)

## Build with the GHC 8.4 Stack YAML
build-8.4:
	$(stack-8.4) $(build)

## Build with the GHC 8.6 Stack YAML
build-8.6:
	$(stack-8.6) $(build)

## Upload the package to Hackage
upload:
	stack upload --no-signature .

# Create ES5 instance

## Run test environment
compose-ES5:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml --project-directory $(elasticsearch_directory) up

## Run test environment in detach mode
compose-ES5-detach-up:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml --project-directory $(elasticsearch_directory) up -d

## Close test environment if run on detach mode
compose-ES5-detach-down:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml --project-directory $(elasticsearch_directory) down

## build the docker compose images
compose-build:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml --project-directory $(elasticsearch_directory) build

## Spawn bash shell in ES5 test container
ES5-shell:
	@LOCAL_USER_ID=${LOCAL_USER_ID} docker-compose -f docker-compose.yml --project-directory $(elasticsearch_directory) exec elasticsearch1 bash

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
