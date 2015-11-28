#!/bin/bash

cabal test
mkdir -p dist/build/hpc
hpc markup --exclude="Main" --exclude="HEphem.TestUtils" --exclude="HEphem.HEphemSpec" --exclude="HEphem.UISpec" --exclude="HEphem.BSParserSpec" spec
mv *.html dist/build/hpc
rm spec.tix
