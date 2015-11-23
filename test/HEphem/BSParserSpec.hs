{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module HEphem.BSParserSpec where

import           HEphem.BSParser
import           HEphem.TestUtils
import           HEphem.Data
import           Text.ParserCombinators.ReadP
import           Test.HUnit
import           Data.Maybe
import           Data.Angle
import           Test.Hspec

spec :: Spec
spec = describe "BrightStar parser" $ do

  it "parses the entire file without error" $ do
    length (filter (\x -> bSpectralType x == " ") brightstarlist) @?= 0

  it "matches values for mirfak" $ do
    bHRNo mirfak @?= 1017
    bMagitude mirfak @=~? 1.79
    fromJust (bUminB mirfak) @=~? 0.37
    bBminV mirfak @=~? 0.48

  it "parses a declination" $ do
    let (Degrees d) = (fst . last) $ readP_to_S readDec "  +49 54 54"
      in d @?= 49.915

  describe "fromHMS" $ do
    it "converts correctly for a test value" $ do
      radians (fromHMS 12 30 30) @=~? Radians ((12 + 30 / 60 + 30 / 3600) * pi * 15 / 180)

