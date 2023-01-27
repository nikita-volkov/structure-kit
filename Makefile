all: test-strictly

format:
	path=structure-kit.cabal && cabal-fmt -c $$path || cabal-fmt -i $$path
	ormolu --mode inplace -c $$(find . -name "*.hs" -not -path "./.stack-snapshot/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./*.stack-work/*" -not -path "./sketches/*")

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
