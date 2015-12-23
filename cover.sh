#!/bin/bash

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
chromium dist/build/hpc/hpc_index.html
