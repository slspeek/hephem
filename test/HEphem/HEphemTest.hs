module HEphem.HEphemTest where

import           HEphem.HEphem
import           Text.ParserCombinators.ReadP

simpleTest :: Bool
simpleTest = True

perseus :: String
perseus = "33   alpha    Per  1017   3 25 26.2   +49 54 54   das     1.79 +0.37 +0.48 F5 Ib"

decl :: String
decl = "   +49 54 54"

testParseAlfaPerseus :: Bool
testParseAlfaPerseus = bHRNo pers == 1017
  where
                       pers = (fst . last) $  readP_to_S star perseus

testParseDeclination :: Bool
testParseDeclination = True
