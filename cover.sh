#!/bin/bash

sed -i -e "146 s/^--//;147 s/\(.*\)/--\1/g" hephem.cabal
rm spec.tix
cabal test
mkdir -p dist/build/hpc
hpc markup  --exclude="Main" \
            --exclude="HEphem.TestUtil" \
            --exclude="HEphem.HEphemSpec" \
            --exclude="HEphem.UISpec" \
            --exclude="HEphem.DataSpec" \
            --exclude="HEphem.ParserUtilSpec" \
            --exclude="HEphem.BSParserSpec" \
            --exclude="HEphem.NGCParserSpec" \
             spec
mv *.html dist/build/hpc
sed -i -e "146 s/\(.*\)/--\1/ ;147 s/^--//;" hephem.cabal
chromium dist/build/hpc/hpc_index.html
