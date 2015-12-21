module HEphem.TestUtil where

import           Data.Time.Calendar
import           Data.Time.Clock
import           HEphem.BSParser
import           HEphem.Data
import           Test.HUnit
import           Text.ParserCombinators.ReadP

(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?) expected actual = expected =~ actual @? assertionMsg
  where
    assertionMsg = "Expected : " ++ show expected ++
                                    "\nActual   : " ++ show actual

parseStar :: String -> BrightStar
parseStar = fst . last . readP_to_S starReadP

mirfakLine :: String
mirfakLine = "  33   alpha    Per  1017   3 25 29     +49 54 50   das     1.79 +0.37 +0.48 F5 Ib"

betelgeuseLine :: String
betelgeuseLine = "  58   alpha    Ori  2061   5 56 02.0   + 7 24 27   ad6     0.50 +2.06 +1.85 M1-M2 Ia-Iab"

pole :: String
pole = "          NorthPole  0000   0 00 00      90 00 10   das     1.79 +0.37 +0.48 F5 Ib"

betelgeuse :: BrightStar
betelgeuse = parseStar betelgeuseLine

mirfak :: BrightStar
mirfak = parseStar mirfakLine

northskypole :: BrightStar
northskypole = parseStar pole

tzero :: UTCTime
tzero = UTCTime (fromGregorian 2015 10 19) 0
