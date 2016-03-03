{-# LANGUAGE TypeSynonymInstances #-}
module HEphem.HEphemSpec where

import           Control.Lens
import           Data.Angle
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

prop_SolveTrigonom :: Double -> Double -> Double -> Property
prop_SolveTrigonom a b c =
  a * a - (c - b) * (b + c) > 0
  ==>
    f s1 =~ 0 && f s2 =~ 0
  where
    ss = solveTrigonom a b c
    s1 = head ss
    s2 = head $ tail ss
    f x = a * sine x + b * cosine x + c

prop_localSiderealtimeToUtcTime :: Double -> Double -> GeoLoc -> Property
prop_localSiderealtimeToUtcTime t lst geo =
  (lst <= 360) && (lst >= 0) && (t > 0) && (t < 1e4)
  ==>
  ((localSiderealtime geo (localSiderealtimeToUtcTime geo testDate (Degrees lst)) - Degrees lst) <= 1e-3)
  where
    testDate = UTCTime (fromGregorian 2001 1 1) (realToFrac t)

prop_heightForAzimuth :: Double -> EqPos -> GeoLoc -> Property
prop_heightForAzimuth lst eq geo =
  (lst < 360) && (lst >= 0)
  ==>
    (hp^.hAltitude) =~ head (heightForAzimuth geo eq (hp^.hAzimuth))
      ||
       (hp^.hAltitude) =~ head ( tail (heightForAzimuth geo eq (hp^.hAzimuth)))
  where
    hp = toHorPosCoord (Degrees lst) geo eq

prop_azimuthForHeight :: Double -> EqPos -> GeoLoc -> Property
prop_azimuthForHeight lst eq geo =
  (lst < 360) && (lst >= 0)
  ==>
  (hp^.hAzimuth) =~ head (azimuthForHeight geo eq (hp^.hAltitude))
                              || (hp^.hAzimuth) =~ head (tail (azimuthForHeight geo eq (hp^.hAltitude)))
  where
    hp = toHorPosCoord (Degrees lst) geo eq

prop_localSiderealtimeFromPos :: Double -> GeoLoc -> EqPos -> Property
prop_localSiderealtimeFromPos lst geo eq =
  (lst <= 360) && (lst > 0)
  ==>
  localSiderealtimeFromPos geo eq hp =~ Degrees  lst
  where
    hp = toHorPosCoord (Degrees lst) geo eq

prop_intersectHeight ::  GeoLoc -> EqPos -> Double -> Property
prop_intersectHeight geo eq al =
  (al >= 0) && (al <= 90)
  ==>
  all (\(lst, hp) -> hp =~ toHorPosCoord lst geo eq) (intersectHeight geo eq (Degrees al))

prop_intersectAzimuth ::  GeoLoc -> EqPos -> Double -> Property
prop_intersectAzimuth geo eq az =
  (az >= 0) && (az <= 360)
  ==>
  all (\(lst, hp) -> hp =~ toHorPosCoord lst geo eq) (intersectAzimuth geo eq (Degrees az))

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

    describe "localSiderealtimeToUtcTime" $
      it "gives next moment with lst" $
        property prop_localSiderealtimeToUtcTime

    describe "heightForAzimuth" $
      it "predict a given horpos" $
        property prop_heightForAzimuth

    describe "azimuthForHeight" $
      it "predict a given horpos" $
        property prop_azimuthForHeight

    describe "solveTrigonom" $
      it "calculates solutions" $
        property prop_SolveTrigonom

    describe "localSiderealtimeFromPos" $
      it "at the time returned the star is at that position" $
        property prop_localSiderealtimeFromPos

    describe "intersectHeight" $
      it "gives lst and pos inline with toHorPosCoord" $
        property prop_intersectHeight

    describe "intersectAzimuth" $
      it "gives lst and pos inline with toHorPosCoord" $
        property prop_intersectAzimuth

    describe "intersectInterval" $ do
      it "should work for ordered intervals" $ do
          intersectInterval (1,2) (3,4) `shouldBe` [];
          intersectInterval (1,2) (1,4) `shouldBe` [(1,2)]
      it "should work for ordered intervals 2" $ do
          intersectInterval (10,20) (0,12) `shouldBe` [(10,12)]
          intersectInterval (10,20) (15,30) `shouldBe` [(15,20)]
      it "should work for left included in right" $
          intersectInterval (160,175) (83,337) `shouldBe` [(160,175)]
      it "should work for reverse ordered intervals" $
        intersectInterval (350,20) (359,4) `shouldBe` [(359,4)]
      it "should work for reverse ordered intervals 2" $
        intersectInterval (350,20) (1,4) `shouldBe` [(1,4)]
      it "should work for reverse ordered intervals 3" $
        intersectInterval (10,20) (320,14) `shouldBe` [(10, 14)]
      it "should work for reverse ordered intervals 4" $
        intersectInterval (90,180) (170,100) `shouldBe` [(170,180), (90, 100)]
