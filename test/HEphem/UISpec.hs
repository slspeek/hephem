module HEphem.UISpec where

import           Data.Angle
import           Data.Maybe
import           Data.Vector.Class
import           Data.Vector.V3
import           GHC.Float
import           HEphem.Data
import           HEphem.TestUtils
import           HEphem.UI
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
import           Test.QuickCheck

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

testScreenIntersects :: Test
testScreenIntersects = TestList
  [testScreenIntersect scr hor p | (scr, hor, p) <-
    [ (flatNorth1, flatNorth, Vector3 1 0 0)
    , (flatNorth1, north, Vector3 1 0 1)
    , (flatNorth1, HorPos (Degrees 45) (Degrees 0), Vector3 1 1 0)
    , (flatNorth1, northEast, Vector3 1 1 (sqrt 2))
    , (northEast1, northEast, Vector3 0.5 0.5 (sqrt 2 / 2))
    , (flatEast1, flatEast, Vector3 0 1 0)
    ]
  ]
  where
    testScreenIntersect scr hor p = TestCase (p @=~? fromJust (screenIntersect scr hor))

prop_ScreenIntersect :: Screen -> HorPos -> Property
prop_ScreenIntersect scr hor = isJust ints ==> ((fromJust ints - origin scr) `vdot` normalVector scr) =~ 0
  where
    ints = screenIntersect scr hor

prop_SolveLinear :: Screen -> (Double, Double) -> Bool
prop_SolveLinear s (x, y) = vmag ((p *| v + q *| w) - i) < 0.1
  where
    (v, w) = grid s
    i = x *| v + y *| w
    (p, q) = fromJust $ solveLinearEq (v, w) i


testRelativeCoords :: Test
testRelativeCoords = TestList
                       [testRelativeCoord s (Vector3 p0 p1 p2) r | (s, (p0, p1, p2), r) <- [ (flatNorth1, (1, 0, 1), (0, 1))
                                                                                           , (flatNorth1, (1, 0, 10), (0, 10))
                                                                                           , (flatNorth1, (1, 0, 0), (0, 0))
                                                                                           , (flatNorth1, (1, 1, 0), (1, 0))
                                                                                           ]]
  where
    testRelativeCoord s p r = TestCase
                                (do
                                   let (x, y) = r
                                   let rc = fromJust $ relativeCoord s p
                                   x @=~? fst rc
                                   y @=~? snd rc)

prop_ScreenCoord :: Screen -> HorPos -> Property
prop_ScreenCoord s hor = isJust (screenCoord s hor) && isJust (screenIntersect s hor) ==>
  (vmag ((origin s + float2Double p *| v + float2Double q *| w) - i) < 0.1)
    where
      (p, q) = fromJust $ screenCoord s hor
      (v, w) = grid s
      i = fromJust $ screenIntersect s hor

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
    
    it "each component has size one" $ property $
      \s -> let (x, y) = grid s
                hasMagOne v = abs (vmag v - 1) < 1.0e-2
            in hasMagOne x && hasMagOne y

  describe "screenIntersect" $
    describe "holds for some easy test values" $
      fromHUnitTest testScreenIntersects;

    it "lays in the plane" $ property  
       prop_ScreenIntersect    

  describe "solveLinearEq" $
    it "can calutate back sum of linear product of the grid vectors" $
      property prop_SolveLinear    

  describe "relativeCoord" $
    describe "holds for some simple test values" $
      fromHUnitTest testRelativeCoords;

  describe "screenCoord" $
    it "screen intersect matches origin plus linear sum of the grid" $
      property prop_SolveLinear    


