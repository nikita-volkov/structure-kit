#!/bin/bash
set -eo pipefail

function drop_first_line_in_file {
  tail -n +2 "$1" > "$1.tmp" && mv "$1.tmp" "$1"
}

time_allocation_report_file="profile/time-allocation-report"
gc_report_file="profile/gc-report"

# Docs on GHC profiling options:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#compiler-options-for-profiling

stack \
--work-dir .profile.stack-work \
build \
--ghc-options "-O2 -rtsopts -fprof-auto" \
--profile \
structure-kit:profile --ta "+RTS -p -po$time_allocation_report_file -s$gc_report_file -V0.0001 -RTS"

drop_first_line_in_file $gc_report_file

profiteur $time_allocation_report_file.prof
