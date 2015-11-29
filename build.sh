#!/bin/bash

rm spec.tix
cabal clean && cabal install --enable-tests && cabal test
