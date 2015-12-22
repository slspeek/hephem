module HEphem.DataSpec where

import           Control.Monad
import           Data.Angle
import           Data.Maybe
import           Data.Vector.Class
import           Data.Vector.V3
import           GHC.Float
import           HEphem.Data
import           HEphem.HEphem
import           HEphem.TestUtil
import           HEphem.UI
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
import           Test.QuickCheck

testSolveAngle :: Test
testSolveAngle = TestList
  [TestCase (solveAngle c s @=~? Radians a) | (c, s, a) <-
    [ (b, b, qp)
    , (-b, b, pi - qp)
    , (-b, -b, pi + qp)
    , (b, -b, 2 * pi - qp)
    ]
  ]
  where
    b = sqrt 2.0 / 2
    qp = pi / 4



testCartesian :: HorPos -> Vector3 -> Test
testCartesian hor v = TestCase $ v @=~? cartesian (hor, 1)

testCartesians :: Test
testCartesians = TestList
                   [testCartesian h v | (h, v) <- [ (north, Vector3 (sqrt 2 / 2) 0 (sqrt 2 / 2))
                                                  , (northEast, Vector3 (1 / 2) (1 / 2) (sqrt 2 / 2))
                                                  ]]

prop_Cartesian_Polair :: Vector3 -> Property
prop_Cartesian_Polair v =  (vmag v < 1000) ==> cartesian (polair v) =~ v

prop_Polair_Cartesian :: (HorPos, Double) -> Property
prop_Polair_Cartesian (h, r) =  r > 1 ==> r =~ r' &&  h =~ h'
  where (h', r') = polair (cartesian (h, r))

-- Main test script
--
spec :: SpecWith ()
spec = describe "Data module" $ do
  describe "solveAngle matches for  test values" $
    fromHUnitTest testSolveAngle

  describe "cartesian" $
    fromHUnitTest testCartesians

  describe "Polair and cartesian" $ do

    it "Cartesian after polair is identity" $
      property prop_Cartesian_Polair

    it "Polair after cartesian is identity" $
      property prop_Polair_Cartesian
