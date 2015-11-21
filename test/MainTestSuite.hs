module Main (main) where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.API

import           HEphem.HEphemTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "HEphem tests"
      [ testGroup "declination" $ hUnitTestToTests testParseDeclination
      , testGroup "brightstar list count" $ hUnitTestToTests testBrightStarList
      , testGroup "parse alpha Perseus" $ hUnitTestToTests testParseMirfak
      , testGroup "sidereal time" $ hUnitTestToTests testSiderealtime
      , testGroup "sidereal time 2" $ hUnitTestToTests testSiderealtime'
      , testGroup "RA angle" $ hUnitTestToTests testRAAngle
      , testGroup "solve angle" $ hUnitTestToTests testSolveAngle
      , testGroup "coord conversion" $ hUnitTestToTests testToHorPos
      , testGroup "grid generation" $ hUnitTestToTests testGrids
      , testGroup "screen intersection" $ hUnitTestToTests testScreenIntersects
      , testGroup "relative coord" $ hUnitTestToTests testRelativeCoords
      , testGroup "cartesian" $ hUnitTestToTests testCartesians
      , testProperty "screenIntersect value lays in the screen-plane" prop_ScreenIntersect
      , testProperty "grid vectors have size 1" prop_Grid_Mag1
      , testProperty "grid vectors inproduct =~ 0" prop_Grid_Orthogonal
      , testProperty "linear solver" prop_SolveLinear
      , testProperty "screenCoord" prop_ScreenCoord
      ]
  ]

