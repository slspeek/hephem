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
      testProperty "" simpleTest
     ,testProperty "parse alpha Perseus" testParseAlfaPerseus
     ,testProperty "parse a declination" testParseDeclination
    ]
  ]

