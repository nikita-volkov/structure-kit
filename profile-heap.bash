#!/bin/bash
set -eo pipefail

function wait_till_exists {
  while ! test -f "$1"; do
    sleep 0.1
  done
}

work_dir=.profile.stack-work
exec=profile
sampling=0.001
max_name_length=70
cost_center=performLookups

rm -f $exec.ps
rm -f $exec.hp

stack \
--work-dir $work_dir \
build \
--ghc-options "-O2 -rtsopts" \
--profile \
structure-kit:$exec --ta "+RTS -h -hc$cost_center -i$sampling -L$max_name_length -RTS"

hp2ps -e8in -c $exec.hp

wait_till_exists "$exec.ps"
open $exec.ps
