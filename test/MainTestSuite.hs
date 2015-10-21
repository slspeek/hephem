module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
 
import HEphem.HEphemTest
 
main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "HEphem tests"
    [
     testGroup "declination" $  hUnitTestToTests   testParseDeclination
     ,testGroup "brightstar list count" $  hUnitTestToTests   testBrightStarList
     ,testGroup "parse alpha Perseus" $ hUnitTestToTests testParseMirfak
     ,testGroup "sidereal time" $ hUnitTestToTests testSiderealtime
     ,testGroup "sidereal time 2" $ hUnitTestToTests testSiderealtime'
     ,testGroup "RA angle" $ hUnitTestToTests testRAAngle
     ,testGroup "solve angle" $ hUnitTestToTests testSolveAngle
     ,testGroup "coord conversion" $ hUnitTestToTests testToHorizontal
    ]
  ]

