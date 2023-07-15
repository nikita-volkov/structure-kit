all: test-strictly

format:
	for path in $$(git diff --staged --name-only -- '*.cabal') $$(git ls-files -om --exclude-standard -- '*.cabal'); do if test -f $$path; then cabal-fmt --no-tabular -c $$path 2> /dev/null || cabal-fmt --no-tabular -i $$path; fi; done
	for path in $$(git diff --staged --name-only -- '*.hs') $$(git ls-files -om --exclude-standard -- '*.hs'); do if test -f $$path; then ormolu -ic $$path; fi; done

build-fast: format
	stack build --fast --test --no-run-tests --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -fno-warn-typed-holes -fdefer-typed-holes"

build-strictly: format
	stack build --test --no-run-tests --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -Werror=incomplete-patterns -Wincomplete-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints"

test-fast: format
	stack build --fast --test --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -fno-warn-typed-holes -fdefer-typed-holes"

test-strictly: format
	stack build --test --ghc-options "-j -threaded +RTS -A128m -n2m -RTS -Werror=incomplete-patterns -Wincomplete-patterns -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints"

run-strictly: build-strictly
	stack run

run-fast: build-fast
	stack run

clean:
	stack clean

build: build-fast

test: test-fast

run: run-fast
