module HEphem.UISpec where

import           Control.Monad
import           Data.Angle
import           Data.Maybe
import           Data.Vector.Class
import           Data.Vector.V3
import           GHC.Float
import           HEphem.Data
import           HEphem.HEphem
import           HEphem.TestUtil
import           HEphem.UI
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.HUnit
import           Test.QuickCheck

flatNorth :: HorPos
flatNorth = HorPos (Degrees 0) (Degrees 0)

zenithNorthEast :: HorPos
zenithNorthEast = HorPos (Degrees 45) (Degrees 90)

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

zenithNorthEast1 :: Screen
zenithNorthEast1 = Screen zenithNorthEast 100


testCartesian :: HorPos -> Vector3 -> Test
testCartesian hor v = TestCase $ v @=~? cartesian (hor, 1)

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

prop_Grid_Ortho :: Screen -> Bool
prop_Grid_Ortho s =  let (x, y) = grid s; snv = normalVector s in (x `vdot` y) =~ 0 && (x `vdot` snv) =~ 0 && (y `vdot` snv) =~ 0

prop_Grid_SizeOne::Screen -> Bool
prop_Grid_SizeOne s = let (x, y) = grid s; hasMagOne v = abs (vmag v - 1) < 1.0e-2 in hasMagOne x && hasMagOne y


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

prop_ScreenCoordToHorPos :: Screen -> (Float, Float) -> Property
prop_ScreenCoordToHorPos s c = isJust identity ==> x =~ x' && y =~ y'
  where
    (x, y) = fromJust identity
    identity = screenCoord s (screenCoordToHorPos s c)
    (x', y') = c

prop_ScreenCoordToHorPos2 :: Screen -> HorPos -> Property
prop_ScreenCoordToHorPos2 s h = isJust c ==> screenCoordToHorPos s (fromJust c) =~ h
  where
    c = screenCoord s h

prop_RelativeCoord :: Screen -> HorPos -> Property
prop_RelativeCoord s hor = isJust (screenIntersect s hor) && isJust (relativeCoord s (fromJust(screenIntersect s hor))) ==>
  vmag (o +  float2Double x *| v + float2Double y *| w - i) < 0.1
    where
      (x, y) = fromJust $ relativeCoord s i
      i = fromJust $ screenIntersect s hor
      o = origin s
      (v, w) = grid s

testWorld :: World
testWorld = World allSkyObjects north1 geoAms (-512, -384) tzero 1 6 Nothing


instance Arbitrary Vector3 where
  arbitrary = liftM3 Vector3 nonZero nonZero nonZero
    where nonZero = suchThat arbitrary (/= 0)

prop_Cartesian_Polair :: Vector3 -> Property
prop_Cartesian_Polair v =  (vmag v < 1000) ==> cartesian (polair v) =~ v

prop_Polair_Cartesian :: (HorPos, Double) -> Property
prop_Polair_Cartesian (h, r) =  r > 1 ==> r =~ r' &&  h =~ h'
  where (h', r') = polair (cartesian (h, r))

-- Main test script
--
spec :: SpecWith ()
spec = describe "UI module" $ do
  describe "cartesian" $
    fromHUnitTest testCartesians

  describe "grid" $ do
    describe "holds for some values" $
      fromHUnitTest testGrids

    it "is orthogonal " $ property
      prop_Grid_Ortho;

    it "each component has size one" $ property
      prop_Grid_SizeOne

    it "is orthogonal in zenithNorthEast" $ property $
      prop_Grid_Ortho zenithNorthEast1;

    it "each component has size one in zenithNorthEast" $ property $
      prop_Grid_SizeOne zenithNorthEast1;

  describe "screenIntersect" $ do

    describe "holds for some easy test values" $
      fromHUnitTest testScreenIntersects;

    it "lays in the plane" $ property
       prop_ScreenIntersect;

    it "lays in the plane for zenithNorthEast" $ property $
       prop_ScreenIntersect zenithNorthEast1

  describe "solveLinearEq" $ do

    it "can calculate back sum of linear product of the grid vectors" $
      property prop_SolveLinear;

    it "can calculate back sum of linear product of the grid vectors in zenithNorthEast" $
      property $ prop_SolveLinear zenithNorthEast1

  describe "relativeCoord" $ do

    describe "holds for some simple test values" $
      fromHUnitTest testRelativeCoords;

    it "gives coords that give back the intersection with the plane" $
      property prop_RelativeCoord;

    it "gives coords that give back the intersection with the plane in zenithNorthEast" $
      property $ prop_RelativeCoord zenithNorthEast1

  describe "screenCoord" $ do

    it "screen intersect matches origin plus linear sum of the grid" $
      property prop_ScreenCoord;

    it "screen intersect matches origin plus linear sum of the grid in zenithNorthEast" $
      property $ prop_ScreenCoord zenithNorthEast1

  describe "Polair and cartesian" $ do

    it "Cartesian after polair is identity" $
      property prop_Cartesian_Polair

    it "Polair after cartesian is identity" $
      property prop_Polair_Cartesian

  describe "screenCoord and screenCoordToHorPos" $ do

    it "screenCoord after screenCoordToHorPos should be the identity" $
      property prop_ScreenCoordToHorPos

    it "screenCoordToHorPos after screenCoord should be the identity" $
      property prop_ScreenCoordToHorPos2
