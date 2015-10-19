module HEphem.HEphemTest where

import           HEphem.HEphem
import           Text.ParserCombinators.ReadP
import           Test.HUnit
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock

class AEq a where
  (=~) :: a -> a -> Bool
instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?) expected actual = expected =~ actual @? assertionMsg
  where
    assertionMsg = "Expected : " ++ show expected ++
                                    "\nActual   : " ++ show actual

perseus :: String
perseus = "  33   alpha    Per  1017   3 25 26.2   +49 54 54   das     1.79 +0.37 +0.48 F5 Ib"

decl ::  String
decl = "  +49 54 54"

longAms = Longitude 4 51 59

testBrightStarList :: Test 
testBrightStarList = TestCase(length ( filter (\x -> bSpectralType x == " ") brightstarlist) @?= 0)

testParseAlfaPerseus :: Test
testParseAlfaPerseus = TestCase (let
                       pers = (fst . last) $  readP_to_S star perseus in
                       do
                       bHRNo pers @?= 1017
                       bMagitude pers @=~? 1.79
                       fromJust ( bUminB pers) @=~? 0.37
                       bBminV pers @=~? 0.48
                       )


testParseDeclination :: Test
testParseDeclination = TestCase (let(Dec d m s) = (fst . last) $ readP_to_S readDec decl in  
                       do
                        d @?= 49 
                        m @?= 54 
                        54 @=~? s 
                       )

testSiderealtime :: Test
testSiderealtime = TestCase ( toMinutesSeconds (siderealtime utc)  @?= (1,48,36))
  where
    utc = UTCTime{utctDay=fromGregorian 2015 10 19, utctDayTime=secondsToDiffTime 0}

testSiderealtime' :: Test
testSiderealtime' = TestCase ( toMinutesSeconds (siderealtime utc)  @?= (0,37,38))
  where
    utc = UTCTime{utctDay=fromGregorian 2015 10 1, utctDayTime=secondsToDiffTime 0}
