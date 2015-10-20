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

instance AEq Horizontal where
  x =~ y = abs (xaz - yaz) < d && abs (xa - ya) < d
    where
      Horizontal (Azimuth xaz) (Altitude xa) = x;
      Horizontal (Azimuth yaz) (Altitude ya) = y;
      d = (1.0e-1::Double)

(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?) expected actual = expected =~ actual @? assertionMsg
  where
    assertionMsg = "Expected : " ++ show expected ++
                                    "\nActual   : " ++ show actual

mirfakLine :: String
mirfakLine = "  33   alpha    Per  1017   3 25 29     +49 54 50   das     1.79 +0.37 +0.48 F5 Ib"

pole  =   "          NorthPole  0000   0 00 00      90 00 00   das     1.79 +0.37 +0.48 F5 Ib"

parseMirfak :: BrightStar
parseMirfak = (fst . last) $ readP_to_S star mirfakLine

decl :: String
decl = "  +49 54 54"

geoAms = GeoLocation (Latitude 52 21 0) (Longitude 4 51 59)

testBrightStarList :: Test
testBrightStarList = TestCase (length (filter (\x -> bSpectralType x == " ") brightstarlist) @?= 0)

testParseMirfak :: Test
testParseMirfak = TestCase
                         (let pers = parseMirfak
                          in do
                            bHRNo pers @?= 1017
                            bMagitude pers @=~? 1.79
                            fromJust (bUminB pers) @=~? 0.37
                            bBminV pers @=~? 0.48)

testParseDeclination :: Test
testParseDeclination = TestCase
                         (let (Dec d m s) = (fst . last) $ readP_to_S readDec decl
                          in do
                            d @?= 49
                            m @?= 54
                            54 @=~? s)

testRAAngle :: Test
testRAAngle = TestCase (angle (RA 12 30 30) @=~? ((12 + 30/60 + 30 /3600) * pi * 15 /180 ))

testSiderealtime :: Test
testSiderealtime = TestCase (toMinutesSeconds (siderealtime utc) @?= (1, 48, 36))
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 19, utctDayTime = secondsToDiffTime 0 }

testSiderealtime' :: Test
testSiderealtime' = TestCase (toMinutesSeconds (siderealtime utc) @?= (0, 37, 38))
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 1, utctDayTime = secondsToDiffTime 0 }

testToHorizontalCoord :: UTCTime -> Horizontal -> Test
testToHorizontalCoord utc hor = TestCase( horizontal geoAms utc mirfak @=~? hor)
  where
    mirfak = parseMirfak
    
testToHorizontal :: Test
testToHorizontal = TestList [ testToHorizontalCoord (f u) (g h) | (u,h) <- [(0,((93, 40, 15),(77, 39, 08))),
                                                                            (1,((130, 17, 32),(86, 21, 34))),
                                                                            (3,((271, 40, 38),(73, 44, 08))),
                                                                            (4,((280, 57, 11),(64, 37, 03))),
                                                                            (6,((296,16,44),(47,15,58)))    ]  
                            ]
  where 
    f x = UTCTime{utctDay=fromGregorian 2015 10 19, utctDayTime=secondsToDiffTime (x * 3600)} 
    g ((d, m, s),(d', m', s')) = Horizontal (Azimuth (todec d m s )) (Altitude (todec d' m' s'))

testToHorizontalCoordPole :: Test
testToHorizontalCoordPole  = TestCase( horizontal geoAms utc npole  @=~? hor)
  where
    utc = UTCTime{utctDay=fromGregorian 2015 10 19, utctDayTime=secondsToDiffTime (6 * 3600) }
    npole = (fst . last ) $ readP_to_S star pole
    hor = Horizontal (Azimuth 180) (Altitude 52.35) 


    
