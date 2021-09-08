#!/bin/bash
set -eo pipefail

function drop_first_line_in_file {
  tail -n +2 "$1" > "$1.tmp" && mv "$1.tmp" "$1"
}

ormolu --mode inplace -c \
-o -XBangPatterns \
-o -XConstraintKinds \
-o -XDataKinds \
-o -XDefaultSignatures \
-o -XDeriveDataTypeable \
-o -XDeriveFoldable \
-o -XDeriveFunctor \
-o -XDeriveGeneric \
-o -XDeriveTraversable \
-o -XDerivingVia \
-o -XDuplicateRecordFields \
-o -XEmptyDataDecls \
-o -XFlexibleContexts \
-o -XFlexibleInstances \
-o -XFunctionalDependencies \
-o -XGADTs \
-o -XGeneralizedNewtypeDeriving \
-o -XLambdaCase \
-o -XLiberalTypeSynonyms \
-o -XMagicHash \
-o -XMultiParamTypeClasses \
-o -XMultiWayIf \
-o -XNoImplicitPrelude \
-o -XNoMonomorphismRestriction \
-o -XOverloadedLabels \
-o -XOverloadedStrings \
-o -XPatternGuards \
-o -XPatternSynonyms \
-o -XParallelListComp \
-o -XQuasiQuotes \
-o -XRankNTypes \
-o -XRecordWildCards \
-o -XScopedTypeVariables \
-o -XStandaloneDeriving \
-o -XTemplateHaskell \
-o -XTupleSections \
-o -XTypeApplications \
-o -XTypeFamilies \
-o -XTypeOperators \
-o -XUnboxedTuples \
$(find . -name '*.hs')

time_allocation_report_file="profile/time-allocation-report"
gc_report_file="profile/gc-report"

stack \
--work-dir .profile.stack-work \
build \
--ghc-options "-O2 -rtsopts -fprof-auto" \
--profile \
structure-kit:profile --ta "+RTS -p -po$time_allocation_report_file -s$gc_report_file -V0.0001 -RTS"

drop_first_line_in_file $gc_report_file

profiteur $time_allocation_report_file.prof
