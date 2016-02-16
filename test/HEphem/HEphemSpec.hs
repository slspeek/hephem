module HEphem.HEphemSpec where

import           Data.Maybe
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

testEquatorialToHorizontal :: Test
testEquatorialToHorizontal = TestList
  [testHorizontalCoord s (mkUTCTime u) (mkHorzontal h) | (s, u, h) <-
      [ (Star mirfak, 0, ((93, 40, 15), (77, 39, 8)))
      , (Star mirfak, 1, ((130, 17, 32), (86, 21, 34)))
      , (Star mirfak, 3, ((271, 40, 38), (73, 44, 8)))
      , (Star mirfak, 4, ((280, 57, 11), (64, 37, 3)))
      , (Star mirfak, 6, ((296, 16, 44), (47, 15, 58)))
      , (Star betelgeuse, 0, ((112, 46, 12), (25, 36, 6)))
      , (Star betelgeuse, 1, ((127, 18, 57), (33, 32, 49)))
      , (Star betelgeuse, 2, ((144, 10, 3), (39, 57, 37)))
      , (Star betelgeuse, 3, ((163, 31, 58), (44, 1, 4)))
      , (Star northskypole, 0, ((0, 0, 0), (52, 20, 0)))
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
lstTV = localSiderealtime geoAms tzero

fixedHorToEq :: HorPos -> EqPos
fixedHorToEq = toEqPosCoord  lstTV geoAms

eqToHorAfterHorToEq:: HorPos -> HorPos
eqToHorAfterHorToEq hor = fixedEqToHor (fixedHorToEq hor)

horToEqAfterEqToHor :: EqPos -> EqPos
horToEqAfterEqToHor eq = fixedHorToEq (fixedEqToHor eq)

prop_FindNear :: Bool
prop_FindNear = and [ equatorial s == (equatorial . fromJust) (findNear visibles (equatorial s) 0.001) | s <- visibles]
  where
    visibles = [s | s<-allSkyObjects, magnitude s < 4]

prop_siderealConv :: Double -> Bool
prop_siderealConv d = siderealConv ( siderealConvInv d )=~ d

spec :: SpecWith ()
spec = describe "HEphem" $
  describe "siderealtime" $ do
    it "siderealtime at 2015 10 19" $ do
      let utc = UTCTime { utctDay = fromGregorian 2015 10 19, utctDayTime = secondsToDiffTime 0 }
      (abs (fromHMS 1 48 36.7204 - siderealtime utc) < 1.0e-2) @?= True

    it "siderealtime at 2015 10 1" $ do
      let utc = UTCTime { utctDay = fromGregorian 2015 10 1, utctDayTime = secondsToDiffTime 0 }
      (abs (fromHMS 0 37 38 - siderealtime utc) < 1.0e-2) @?= True

    describe "equatorialToHorizontal for a series of test values" $
      fromHUnitTest testEquatorialToHorizontal

    describe "horizontalToEquatorial" $ do
      it "is left inverse of equatorialToHorizontal" $ property
        prop_horToEqAfterEqToHor;

      it "is right inverse of equatorialToHorizontal" $ property
        prop_eqToHorAfterHorToEq;

    describe "findNear" $
      it "finds a skyobject on its own coords" $ property
        prop_FindNear

    describe "timeInterval" $
      it "gives [0, 10, 20] for t = 0, d = 20, n = 10" $
        timeInterval 0 20 10 `shouldBe` [0, 10, 20]

    describe "siderealConv{,Inv}" $
      it "are inverses" $ property
        prop_siderealConv
