{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module HEphem.HEphemTest where

import           HEphem.HEphem
import           Text.ParserCombinators.ReadP
import           Test.HUnit
import           Test.QuickCheck
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Vector.V3
import           Data.Vector.Class
import           GHC.Float
import           Data.Angle

(@=~?) :: (Show a, AEq a) => a -> a -> Assertion
(@=~?) expected actual = expected =~ actual @? assertionMsg
  where
    assertionMsg = "Expected : " ++ show expected ++
                                    "\nActual   : " ++ show actual

parseStar :: String -> BrightStar
parseStar = fst . last . readP_to_S star

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

decl :: String
decl = "  +49 54 54"

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
                         (let (Degrees d) = (fst . last) $ readP_to_S readDec decl
                          in d @?= 49.915 )

testRAAngle :: Test
testRAAngle = TestCase (radians ( fromHMS 12 30 30) @=~? Radians ((12 + 30 / 60 + 30 / 3600) * pi * 15 / 180))

testSiderealtime :: Test
testSiderealtime = TestCase ((abs (fromHMS 1 48 36.7204 - siderealtime utc) < 0.01) @?= True )
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 19, utctDayTime = secondsToDiffTime 0 }

testSiderealtime' :: Test
testSiderealtime' = TestCase (abs(siderealtime utc - fromHMS 0 37 38) < 0.01 @?= True)
  where
    utc = UTCTime { utctDay = fromGregorian 2015 10 1, utctDayTime = secondsToDiffTime 0 }

testToHorPosCoord :: BrightStar -> UTCTime -> HorPos -> Test
testToHorPosCoord bstar utc hor = TestCase (snd (horizontal geoAms utc bstar) @=~? hor)

testSolveAngle :: Test
testSolveAngle = TestList
                   [TestCase (solveAngle c s @=~? Radians a) | (c, s, a) <- [ (b, b, qp)
                                                                    , (-b, b, pi - qp)
                                                                    , (-b, -b, pi + qp)
                                                                    , (b, -b, 2 * pi - qp)
                                                                    ]]
  where
    b = sqrt 2.0 / 2
    qp = pi / 4

testToHorPos :: Test
testToHorPos = TestList
                     [testToHorPosCoord s (mkUTCTime u) (mkHorzontal h) | (s, u, h) <- [ (mirfak, 0, ((93, 40, 15), (77, 39, 8)))
                                                                                           , (mirfak, 1, ((130, 17, 32), (86, 21, 34)))
                                                                                           , (mirfak, 3, ((271, 40, 38), (73, 44, 8)))
                                                                                           , (mirfak, 4, ((280, 57, 11), (64, 37, 3)))
                                                                                           , (mirfak, 6, ((296, 16, 44), (47, 15, 58)))
                                                                                           , (betelgeuse, 0, ((112, 46, 12), (25, 36, 6)))
                                                                                           , (betelgeuse, 1, ((127, 18, 57), (33, 32, 49)))
                                                                                           , (betelgeuse, 2, ((144, 10, 3), (39, 57, 37)))
                                                                                           , (betelgeuse, 3, ((163, 31, 58), (44, 1, 4)))
                                                                                           , (northskypole, 0, ((0, 0, 0), (52, 20, 0)))
                                                                                           ]]
  where
    mkUTCTime x = UTCTime
      { utctDay = fromGregorian 2015 10 19
      , utctDayTime = secondsToDiffTime (x * 3600)
      }
    mkHorzontal ((d, m, s), (d', m', s')) = HorPos  (fromDMS d m s) (fromDMS d' m' s')

flatNorth :: HorPos
flatNorth = HorPos (Degrees 0) (Degrees 0)

zenithNorth :: HorPos
zenithNorth = HorPos (Degrees 0) (Degrees 90)

flatEast :: HorPos
flatEast = HorPos (Degrees 90) (Degrees 0)

flatNorth1 :: Screen
flatNorth1 = Screen flatNorth 1

flatEast1 :: Screen
flatEast1 = Screen flatEast 1

zenithNorth1 :: Screen
zenithNorth1 = Screen zenithNorth 1

north :: HorPos
north = HorPos (Degrees 0) (Degrees 45)

north1 :: Screen
north1 = Screen north 1

northEast :: HorPos
northEast = HorPos (Degrees 45) (Degrees 45)

northEast1 :: Screen
northEast1 = Screen northEast 1

testCartesian :: HorPos -> Vector3 -> Test
testCartesian hor v = TestCase $ v @=~? cartesian hor

testCartesians :: Test
testCartesians = TestList
                   [testCartesian h v | (h, v) <- [ (north, Vector3
                                                              { v3x = sqrt 2 / 2
                                                              , v3y = 0
                                                              , v3z = sqrt 2 / 2
                                                              })
                                                  , (northEast, Vector3
                                                                  { v3x = 1 / 2
                                                                  , v3y = 1 / 2
                                                                  , v3z = sqrt 2 / 2
                                                                  })
                                                  ]]

testGrid :: Screen -> (Vector3, Vector3) -> Test
testGrid scr vs = TestCase
                    (do
                       fst vs @=~? v
                       snd vs @=~? w)
  where
    (v, w) = grid scr

testGrids :: Test
testGrids = TestList
              [testGrid scr v | (scr, v) <- [ (flatNorth1, (Vector3 { v3x = 0, v3y = 1, v3z = 0 }, Vector3
                                                                                                     { v3x = 0
                                                                                                     , v3y = 0
                                                                                                     , v3z = 1
                                                                                                     }))
                                            , (flatEast1, (Vector3 { v3x = -1, v3y = 0, v3z = 0 }, Vector3
                                                                                                       { v3x = 0
                                                                                                       , v3y = 0
                                                                                                       , v3z = 1
                                                                                                       }))
                                            , (north1, (Vector3 { v3x = 0, v3y = 1, v3z = 0 }, Vector3
                                                                                                 { v3x = -sqrt
                                                                                                            2 / 2
                                                                                                 , v3y = 0
                                                                                                 , v3z = sqrt
                                                                                                           2 / 2
                                                                                                 }))
                                            , (northEast1, (Vector3
                                                              { v3x = -sqrt 2 / 2
                                                              , v3y = sqrt 2 / 2
                                                              , v3z = 0
                                                              }, Vector3
                                                                   { v3x = (-1) / 2
                                                                   , v3y = -1 / 2
                                                                   , v3z = sqrt 2 / 2
                                                                   }))
                                            , (zenithNorth1, (Vector3 { v3x = 0, v3y = 1, v3z = 0 }, Vector3
                                                                                                       { v3x = -1
                                                                                                       , v3y = 0
                                                                                                       , v3z = 0
                                                                                                       }))
                                            ]]

testScreenIntersect :: Screen -> HorPos -> Vector3 -> Test
testScreenIntersect scr hor p = TestCase (p @=~? fromJust (screenIntersect scr hor))

testScreenIntersects :: Test
testScreenIntersects = TestList
                         [testScreenIntersect scr hor p | (scr, hor, p) <- [ (flatNorth1, flatNorth, Vector3
                                                                                                       { v3x = 1
                                                                                                       , v3y = 0
                                                                                                       , v3z = 0
                                                                                                       })
                                                                           , (flatNorth1, north, Vector3
                                                                                                   { v3x = 1
                                                                                                   , v3y = 0
                                                                                                   , v3z = 1
                                                                                                   })
                                                                           , (flatNorth1, HorPos
                                                                                             (Degrees
                                                                                                45)
                                                                                             (Degrees
                                                                                                0), Vector3
                                                                                                       { v3x = 1
                                                                                                       , v3y = 1
                                                                                                       , v3z = 0
                                                                                                       })
                                                                           , (flatNorth1, northEast, Vector3
                                                                                                       { v3x = 1
                                                                                                       , v3y = 1
                                                                                                       , v3z = sqrt
                                                                                                                 2
                                                                                                       })
                                                                           , (northEast1, northEast, Vector3
                                                                                                       { v3x = 0.5
                                                                                                       , v3y = 0.5
                                                                                                       , v3z = sqrt
                                                                                                                 2 / 2
                                                                                                       })
                                                                           , (flatEast1, flatEast, Vector3
                                                                                                     { v3x = 0
                                                                                                     , v3y = 1
                                                                                                     , v3z = 0
                                                                                                     })
                                                                           ]]

prop_Grid_Mag1 :: Screen -> Bool
prop_Grid_Mag1 s = (abs (vmag x - 1) < 1.0e-2) && (abs (vmag y - 1) < 1.0e-2)
  where
    (x, y) = grid s

prop_Grid_Orthogonal :: Screen -> Bool
prop_Grid_Orthogonal s = (x `vdot` y) =~ 0 && (x `vdot` snv) =~ 0 && (y `vdot` snv) =~ 0
  where
    (x, y) = grid s
    snv = normalVector s

prop_ScreenCoord :: Screen -> HorPos -> Property
prop_ScreenCoord s hor = isJust (screenCoord s hor) && isJust (screenIntersect s hor)
                                                       ==> (vmag
                                                              ((origin s +
                                                                float2Double p *| v +
                                                                float2Double q *| w) - i) < 0.1)
  where
    (p, q) = fromJust $ screenCoord s hor
    (v, w) = grid s
    i = fromJust $ screenIntersect s hor

prop_ScreenIntersect :: Screen -> HorPos -> Property 
prop_ScreenIntersect scr hor = isJust ints ==> ((fromJust ints - origin scr) `vdot` normalVector scr) =~ 0
  where
    ints = screenIntersect scr hor

testRelativeCoord :: Screen -> Vector3 -> (Float, Float) -> Test
testRelativeCoord s p r = TestCase
                            (do
                               let (x, y) = r
                               let rc = relativeCoord s p
                               x @=~? fst rc
                               y @=~? snd rc)

testRelativeCoords :: Test
testRelativeCoords = TestList
                       [testRelativeCoord s (Vector3 p0 p1 p2) r | (s, (p0, p1, p2), r) <- [ (flatNorth1, (1, 0, 1), (0, 1))
                                                                                           , (flatNorth1, (1, 0, 10), (0, 10))
                                                                                           , (flatNorth1, (1, 0, 0), (0, 0))
                                                                                           , (flatNorth1, (1, 1, 0), (1, 0))
                                                                                           ]]


