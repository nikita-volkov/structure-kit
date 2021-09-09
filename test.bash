#!/bin/bash
set -eo pipefail

stack \
--work-dir .test.stack-work \
build \
--fast \
--test \
structure-kit:test
