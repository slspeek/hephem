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
     ,testGroup "parse alpha Perseus" $ hUnitTestToTests testParseAlfaPerseus
    ]
  ]

