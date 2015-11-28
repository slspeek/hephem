{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HEphem.BSParserSpec where

import           Data.Angle
import           Data.Maybe
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.TestUtil
import           Test.Hspec
import           Test.HUnit
import           Text.ParserCombinators.ReadP

spec :: Spec
spec = describe "BrightStar parser" $ do

  it "parses the entire file without error" $
    length (filter (\x -> bSpectralType x == " ") brightstarlist) @?= 0

  it "matches values for mirfak" $ do
    bHRNo mirfak @?= 1017
    bMagitude mirfak @=~? 1.79
    fromJust (bUminB mirfak) @=~? 0.37
    bBminV mirfak @=~? 0.48

  it "parses a declination" $
    let (Degrees d) = (fst . last) $ readP_to_S readDec "  +49 54 54"
      in d @?= 49.915

  describe "fromHMS" $
    it "converts correctly for a test value" $
      radians (fromHMS 12 30 30) @=~? Radians ((12 + 30 / 60 + 30 / 3600) * pi * 15 / 180)
