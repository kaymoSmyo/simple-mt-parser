#!/bin/bash

cabal-fmt -i simple-mt-parser.cabal
cabal build
cabal test