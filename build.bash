#!/bin/bash
set -eo pipefail

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

stack build --fast --test \
--ta "--quickcheck-tests 1 --quickcheck-verbose -j1 -p \"Inserting new entry after cap is reached produces evicted entry\" --quickcheck-replay 0"
