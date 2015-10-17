#!/bin/bash

cabal clean && cabal install --enable-tests && cabal test
