module HEphem.ParserUtilSpec
    (spec) where

import           Data.Angle
import           HEphem.ParserUtil
import           HEphem.TestUtil
import           Test.Hspec

spec :: Spec
spec =
  describe "ParserUtil" $ do

    describe "fromHMS" $
      it "converts correctly for a test value" $
        radians (fromHMS 12 30 30) @=~? Radians ((12 + 30 / 60 + 30 / 3600) * pi * 15 / 180)
