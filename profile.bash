#!/bin/bash
set -eo pipefail

target=loc
ghc_options="-fno-prof-auto -O2 -rtsopts -j6 +RTS -A128m -n2m -RTS"
time_allocation_report_file="reports/$target"
gc_report_file="reports/$target.gc"

function drop_first_line_in_file {
  tail -n +2 "$1" > "$1.tmp" && mv "$1.tmp" "$1"
}

# Docs on GHC profiling options:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#compiler-options-for-profiling

mkdir -p reports

stack \
--work-dir .profile.stack-work \
build \
--ghc-options "$ghc_options" \
--profile \
structure-kit:profile-$target --ta "+RTS -p -po$time_allocation_report_file -s$gc_report_file -V0.0001 -RTS"

drop_first_line_in_file $gc_report_file

profiteur $time_allocation_report_file.prof
