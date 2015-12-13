module HEphem.HEphemSpec where

import           Data.Angle
import           Data.Time.Calendar
import           Data.Time.Clock
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.HEphem
import           HEphem.TestUtil
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
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

testEquatorialToHorizontal :: Test
testEquatorialToHorizontal = TestList
  [testHorizontalCoord s (mkUTCTime u) (mkHorzontal h) | (s, u, h) <-
      [ (mirfak, 0, ((93, 40, 15), (77, 39, 8)))
      , (mirfak, 1, ((130, 17, 32), (86, 21, 34)))
      , (mirfak, 3, ((271, 40, 38), (73, 44, 8)))
      , (mirfak, 4, ((280, 57, 11), (64, 37, 3)))
      , (mirfak, 6, ((296, 16, 44), (47, 15, 58)))
      , (betelgeuse, 0, ((112, 46, 12), (25, 36, 6)))
      , (betelgeuse, 1, ((127, 18, 57), (33, 32, 49)))
      , (betelgeuse, 2, ((144, 10, 3), (39, 57, 37)))
      , (betelgeuse, 3, ((163, 31, 58), (44, 1, 4)))
      , (northskypole, 0, ((0, 0, 0), (52, 20, 0)))
      ]
  ]
  where
    testHorizontalCoord bstar utc hor = TestCase (equatorialToHorizontal geoAms utc (equatorial bstar) @=~? hor)
    mkUTCTime x = UTCTime
      { utctDay = fromGregorian 2015 10 19
      , utctDayTime = secondsToDiffTime (x * 3600)
      }
    mkHorzontal ((d, m, s), (d', m', s')) = HorPos (fromDMS d m s) (fromDMS d' m' s')

prop_horToEqAfterEqToHor :: EqPos -> Bool
prop_horToEqAfterEqToHor eq = horToEqAfterEqToHor eq =~ eq

prop_eqToHorAfterHorToEq :: HorPos -> Bool
prop_eqToHorAfterHorToEq hor =  eqToHorAfterHorToEq hor =~ hor

fixedEqToHor :: EqPos -> HorPos
fixedEqToHor = toHorPosCoord lstTV geoAms

lstTV::Deg
lstTV = localSiderealtime geoAms (UTCTime (fromGregorian 2015 10 19) 0)

fixedHorToEq :: HorPos -> EqPos
fixedHorToEq = toEqPosCoord  lstTV geoAms

eqToHorAfterHorToEq:: HorPos -> HorPos
eqToHorAfterHorToEq hor = fixedEqToHor (fixedHorToEq hor)

horToEqAfterEqToHor :: EqPos -> EqPos
horToEqAfterEqToHor eq = fixedHorToEq (fixedEqToHor eq)

spec :: SpecWith ()
spec = describe "HEphem" $
  describe "siderealtime" $ do
    it "siderealtime at 2015 10 19" $ do
      let utc = UTCTime { utctDay = fromGregorian 2015 10 19, utctDayTime = secondsToDiffTime 0 }
      (abs (fromHMS 1 48 36.7204 - siderealtime utc) < 1.0e-2) @?= True

    it "siderealtime at 2015 10 1" $ do
      let utc = UTCTime { utctDay = fromGregorian 2015 10 1, utctDayTime = secondsToDiffTime 0 }
      (abs (fromHMS 0 37 38 - siderealtime utc) < 1.0e-2) @?= True

    describe "solveAngle matches for  test values" $
      fromHUnitTest testSolveAngle

    describe "equatorialToHorizontal for a series of test values" $
      fromHUnitTest testEquatorialToHorizontal

    describe "horizontalToEquatorial" $ do
      it "is left inverse of equatorialToHorizontal" $ property
        prop_horToEqAfterEqToHor;

      it "is right inverse of equatorialToHorizontal" $ property
        prop_eqToHorAfterHorToEq;
