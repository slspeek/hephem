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
      Horizontal (Azimuth xaz) (Altitude xa) = x
      Horizontal (Azimuth yaz) (Altitude ya) = y
      d = 0.1 :: Double

(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?) expected actual = expected =~ actual @? assertionMsg
  where
    assertionMsg = "Expected : " ++ show expected ++
                                    "\nActual   : " ++ show actual

parseStar :: String -> BrightStar
parseStar = fst . last . readP_to_S star

mirfakLine :: String
mirfakLine = "  33   alpha    Per  1017   3 25 29     +49 54 50   das     1.79 +0.37 +0.48 F5 Ib"

betelgeuseLine = "  58   alpha    Ori  2061   5 56 02.0   + 7 24 27   ad6     0.50 +2.06 +1.85 M1-M2 Ia-Iab"
pole = "          NorthPole  0000   0 00 00      90 00 10   das     1.79 +0.37 +0.48 F5 Ib"

betelgeuse :: BrightStar
betelgeuse = parseStar betelgeuseLine

mirfak :: BrightStar
mirfak = parseStar mirfakLine

northskypole :: BrightStar
northskypole = parseStar pole

decl :: String
decl = "  +49 54 54"

geoAms = GeoLocation (Latitude 52 21 0) (Longitude 4 51 59)

testBrightStarList :: Test
testBrightStarList = TestCase (length (filter (\x -> bSpectralType x == " ") brightstarlist) @?= 0)

testParseMirfak :: Test
testParseMirfak = TestCase
                    (let pers = mirfak
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
testRAAngle = TestCase (angle (RA 12 30 30) @=~? ((12 + 30 / 60 + 30 / 3600) * pi * 15 / 180))

testSiderealtime :: Test
testSiderealtime = TestCase (toMinutesSeconds (siderealtime utc) @?= (1, 48, 36))
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 19, utctDayTime = secondsToDiffTime 0 }

testSiderealtime' :: Test
testSiderealtime' = TestCase (toMinutesSeconds (siderealtime utc) @?= (0, 37, 38))
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 1, utctDayTime = secondsToDiffTime 0 }

testToHorizontalCoord :: BrightStar -> UTCTime -> Horizontal -> Test
testToHorizontalCoord bstar utc hor = TestCase (snd ( horizontal geoAms utc bstar) @=~? hor)
    
testSolveAngle :: Test
testSolveAngle = TestList [ TestCase (solveAngle c s @=~? a)| (c,s,a) <- [(b,b,qp), (-b,b, pi-qp), (-b,-b,pi+qp),(b, -b, 2*pi - qp)]] 
  where 
    b = sqrt 2.0 / 2
    qp = pi/4

testToHorizontal :: Test
testToHorizontal = TestList [ testToHorizontalCoord s (mkUTCTime u) (mkHorzontal h) | (s, u, h) <- [(mirfak, 0,((93, 40, 15),(77, 39, 08))),
                                                                            (mirfak, 1,((130, 17, 32),(86, 21, 34))),
                                                                            (mirfak, 3,((271, 40, 38),(73, 44, 08))),
                                                                            (mirfak, 4,((280, 57, 11),(64, 37, 03))),
                                                                            (mirfak, 6,((296,16,44),(47,15,58))),
                                                                            (betelgeuse, 0, ((112, 46, 12),(25,36,06))),
                                                                            (betelgeuse, 1, ((127,18,57),(33,32,49))),
                                                                            (betelgeuse, 2, ((144,10,03),(39,57,37))),
                                                                            (betelgeuse, 3, ((163,31,58),(44,01,04))),
                                                                            (northskypole, 0, ((0,0,0),(52, 20, 00)))   ]  
                            ]
  where 
    mkUTCTime x = UTCTime{utctDay=fromGregorian 2015 10 19, utctDayTime=secondsToDiffTime (x * 3600)} 
    mkHorzontal ((d, m, s),(d', m', s')) = Horizontal (Azimuth (todec d m s )) (Altitude (todec d' m' s'))


    
