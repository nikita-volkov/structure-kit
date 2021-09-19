#!/bin/bash
set -eo pipefail

stack \
--work-dir .bench.stack-work \
build \
--ghc-options "-O2 -threaded" \
--bench \
--ba "-s -m pattern lru-ord-cache" \
structure-kit:bench
