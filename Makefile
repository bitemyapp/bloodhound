stack = STACK_YAML='stack.yaml' stack

build:
	stack build

build-validate:
	stack build --fast --ghc-options '-Wall -Werror'

ghci:
	stack ghci

test: echo-warn
	stack test

test-rerun: echo-warn
	stack test --test-arguments "-r"

test-ghci:
	stack ghci bloodhound:test:bloodhound-tests

ghcid:
	ghcid -c "$(stack) ghci bloodhound:lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is bloodhound:test:bloodhound-tests"

ghcid-validate:
	ghcid -c "$(stack) ghci bloodhound:lib --test --ghci-options='-Werror -fobject-code -fno-warn-unused-do-bind' --main-is bloodhound:test:bloodhound-tests"

weeder:
	weeder . --build

# hlint --default > .hlint.yaml
hlint:
	hlint .

hlint-watch:
	sos src/ -c "hlint ." -p "src/(.*)\.hs"

mod-build:
	stack build --ghc-options '+RTS -A128M -RTS'

echo-warn:
	@echo "Make certain you have an elasticsearch instance on localhost:9200 !"

7.8-build:
	STACK_YAML="stack-7.8.yaml" stack build

7.8-test: echo-warn
	STACK_YAML="stack-7.8.yaml" stack test

7.10-build:
	STACK_YAML="stack-7.10.yaml" stack build

7.10-test: echo-warn
	STACK_YAML="stack-7.10.yaml" stack test

7.10-test-ES1:
	STACK_YAML="stack-7.10.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES1

7.10-test-ES5:
	STACK_YAML="stack-7.10.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES5

8.0-test-ES1:
	STACK_YAML="stack-8.0.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES1

8.0-test-ES5:
	STACK_YAML="stack-8.0.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES5

8.2-test-ES1:
	STACK_YAML="stack.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES1

8.2-test-ES5:
	STACK_YAML="stack.yaml" stack test --fast bloodhound:test:bloodhound-tests --test-arguments="--qc-max-success 500" --flag bloodhound:ES5

8.0-build:
	STACK_YAML="stack-8.0.yaml" stack build

8.2-build:
	STACK_YAML="stack-8.2.yaml" stack build

module-touch:
	touch src/Database/V1/Bloodhound/Types.hs
	touch src/Database/V5/Bloodhound/Types.hs

upload:
	stack upload --no-signature .
