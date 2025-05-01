#!/bin/bash

cabal-fmt -i start-haskell.cabal
cabal build
cabal test