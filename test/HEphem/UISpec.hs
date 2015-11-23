module HEphem.UISpec where

import           HEphem.UI
import           HEphem.Data
import           HEphem.TestUtils
import           Test.HUnit
import           Test.QuickCheck
import           Data.Maybe
import           Data.Vector.V3
import           Data.Vector.Class
import           GHC.Float
import           Data.Angle
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)

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
                   [testCartesian h v | (h, v) <- [ (north, Vector3 (sqrt 2 / 2) 0 (sqrt 2 / 2))
                                                  , (northEast, Vector3 (1 / 2) (1 / 2) (sqrt 2 / 2))
                                                  ]]

testGrids :: Test
testGrids = TestList
              [testGrid scr v | (scr, v) <- [ (flatNorth1, (Vector3 0 1 0, Vector3 0 0 1))
                                            , (flatEast1, (Vector3 (-1) 0 0, Vector3 0 0 1))
                                            , (north1, (Vector3 0 1 0, Vector3 (-sqrt 2 / 2) 0
                                                                         (sqrt 2 / 2)))
                                            , (northEast1, (Vector3 (-sqrt 2 / 2) (sqrt 2 / 2) 0, Vector3
                                                                                                    (-1 / 2)
                                                                                                    (-1 / 2)
                                                                                                    (sqrt
                                                                                                       2 / 2)))
                                            , (zenithNorth1, (Vector3 0 1 0, Vector3 (-1) 0 0))
                                            ]]
  where
    testGrid scr vs = TestCase
                        (let (v, w) = grid scr
                         in do
                           fst vs @=~? v
                           snd vs @=~? w)

spec :: SpecWith ()
spec = describe "UI module" $ do
  describe "cartesian" $
    fromHUnitTest testCartesians

  describe "grid" $ do
    describe "holds for some values" $
      fromHUnitTest testGrids

    it "is orthogonal " $ property $
      \s -> let (x, y) = grid s
                snv = normalVector s
            in (x `vdot` y) =~ 0 && (x `vdot` snv) =~ 0 && (y `vdot` snv) =~ 0

testScreenIntersect :: Screen -> HorPos -> Vector3 -> Test
testScreenIntersect scr hor p = TestCase (p @=~? fromJust (screenIntersect scr hor))

testScreenIntersects :: Test
testScreenIntersects = TestList
                         [testScreenIntersect scr hor p | (scr, hor, p) <- [ (flatNorth1, flatNorth, Vector3
                                                                                                       1
                                                                                                       0
                                                                                                       0)
                                                                           , (flatNorth1, north, Vector3
                                                                                                   1
                                                                                                   0
                                                                                                   1)
                                                                           , (flatNorth1, HorPos
                                                                                            (Degrees
                                                                                               45)
                                                                                            (Degrees
                                                                                               0), Vector3
                                                                                                     1
                                                                                                     1
                                                                                                     0)
                                                                           , (flatNorth1, northEast, Vector3
                                                                                                       1
                                                                                                       1
                                                                                                       (sqrt
                                                                                                          2))
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
                               let rc = fromJust $ relativeCoord s p
                               x @=~? fst rc
                               y @=~? snd rc)

testRelativeCoords :: Test
testRelativeCoords = TestList
                       [testRelativeCoord s (Vector3 p0 p1 p2) r | (s, (p0, p1, p2), r) <- [ (flatNorth1, (1, 0, 1), (0, 1))
                                                                                           , (flatNorth1, (1, 0, 10), (0, 10))
                                                                                           , (flatNorth1, (1, 0, 0), (0, 0))
                                                                                           , (flatNorth1, (1, 1, 0), (1, 0))
                                                                                           ]]

prop_SolveLinear :: Screen -> (Double, Double) -> Bool
prop_SolveLinear s (x, y) = vmag ((p *| v + q *| w) - i) < 0.1
  where
    (v, w) = grid s
    i = x *| v + y *| w
    (p, q) = fromJust $ solveLinearEq (v, w) i

