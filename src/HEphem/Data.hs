{-# LANGUAGE ExistentialQuantification, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HEphem.Data where

import           Data.Angle
import           Test.QuickCheck
import           Data.Vector.V3
import           Control.Lens                     hiding (element)
import           Text.Printf
import           Data.Vector.Class
import           Data.Fixed            (mod', div')

type Deg = Degrees Double

instance Arbitrary Deg where
  arbitrary =
    do
      d <- suchThat arbitrary (\x -> x >= 0 && x <= 360)
      return $ Degrees d

type Interval = (Deg, Deg)

standardizeDeg:: Deg -> Deg
standardizeDeg (Degrees d) = Degrees $ d `mod'` 360

undeg:: Deg -> Double
undeg (Degrees s) = s

toMinutesSeconds :: Deg -> (Int, Int, Int)
toMinutesSeconds (Degrees d) = (i, m, s)
  where
    i = floor d
    r = d - fromIntegral i
    m = r `div'` (1 / 60)
    r' = r - fromIntegral m * (1 / 60)
    s = r' `div'` (1 / 3600)

printDeg :: Deg -> String
printDeg deg = printf "%d\x00B0 %d\"%d'" d m s
  where
    (d, m, s) = toMinutesSeconds deg

printDegAsTime :: Deg -> String
printDegAsTime deg = printf "%dh %dm%ds" d m s
  where
    (d, m, s) = toMinutesSeconds (deg/15)

data EqPos = EqPos { eRA,eDec :: Deg }
  deriving (Eq, Show)

instance Arbitrary EqPos where
  arbitrary = do
    ra <- suchThat arbitrary (\x -> x >= 0 && x <= 360)
    d <- suchThat arbitrary (\x -> x >= -89 && x < 89)
    return $ EqPos (Degrees ra) (Degrees d)

data HorPos = HorPos { _hAzimuth,_hAltitude :: Deg }
  deriving (Eq, Show)

instance Arbitrary HorPos where
  arbitrary = do
    az <- suchThat arbitrary (\x -> x >= 0 && x <= 360)
    al <- suchThat arbitrary (\x -> x >= 0 && x <= 90)
    return $ HorPos (Degrees az) (Degrees al)

makeLenses ''HorPos

instance Ord HorPos where
  compare a b = compare (a ^. hAltitude) (b ^. hAltitude)

data GeoLoc = GeoLoc { _gLatitude,_gLongitude :: Deg }
  deriving (Eq, Show)

instance Arbitrary GeoLoc where
  arbitrary = do
    az <- suchThat arbitrary (\x -> x >= -90 && x <= 90)
    al <- suchThat arbitrary (\x -> x >= -180 && x <= 180)
    return $ GeoLoc (Degrees az) (Degrees al)


makeLenses ''GeoLoc

data BrightStar = BrightStar
    { bFlamsteed::Maybe Int
    , bBayer::String
    , bConst::String
    , bHRNo         :: Int
    , bRA           :: Deg
    , bDec          :: Deg
    , bNotes        :: String
    , bMag     :: Float
    , bUminB        :: Maybe Float
    , bBminV        :: Float
    , bSpectralType :: String
    }
    deriving (Show,Eq)

data NGCObject = NGCObject
    { nID :: String
    , nPGC:: String
    , nMessier:: String
    , nType:: String
    , nClass:: String
    , nRA :: Deg
    , nDec :: Deg
    , nMag :: Float
     } deriving (Show,Eq)

data SkyObject = NGC NGCObject|Star BrightStar deriving (Show)

instance Eq SkyObject where
  Star a == Star b = a == b
  NGC  a == NGC b  = a == b
  _      ==  _     = False

equatorial:: SkyObject -> EqPos
equatorial (Star (BrightStar  _ _ _ _ r d _ _ _ _ _)) = EqPos r d
equatorial (NGC (NGCObject _ _ _ _ _ r d _ )) = EqPos r d

magnitude:: SkyObject -> Float
magnitude (Star a) = bMag a
magnitude (NGC  a) = nMag a

description :: SkyObject -> String
description (Star(BrightStar f b c hr _ _ _ m _ _ _)) =
   case f of Nothing -> if b == "" then if c == "" then printf "HR# %v Mag %.1f" hr m
                                                   else printf "%s HR# %v Mag %.1f" c hr m
                                   else printf "%s %s HR# %v Mag %.1f" b c hr m

             Just fl -> printf "%d %s %s HR# %v Mag %.1f" fl b c hr m
description (NGC (NGCObject i _ me t _ _ _ m )) =
  if me == "" then printf "%s Type %s Mag %.1f" i t m
               else printf "%s %s Type %s Mag %.1f" me i t m


data Rectangle = Rectangle{
    _rAzimuth::Interval,
    _rAltitude::Interval
    } deriving (Show)

makeLenses ''Rectangle

instance Arbitrary Rectangle where
  arbitrary =
    do
      az0 <- suchThat arbitrary (\x -> x >= 0 && x < 360)
      az1 <- suchThat arbitrary (\x -> x >= 0 && x < 360)
      al0 <- suchThat arbitrary (\x -> x >= 0 && x < 90)
      al1 <- suchThat arbitrary (\x -> x >= al0 && x < 90)
      return $ Rectangle (az0, az1) (al0, al1)

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where -- for solveTrigonom is it so big
  x =~ y = abs (x - y) < (1.0e-2 :: Double)

instance AEq Deg where
  x =~ y = abs (x - y) < (1.0e-4 :: Deg)

instance AEq HorPos where
  (HorPos x y) =~ (HorPos x' y') =
     vmag (cartesian (HorPos x y, 1) - cartesian (HorPos x' y', 1)) < d
    where
      -- has too be so big for manual test data
      d = 1e-2

instance AEq EqPos where
  (EqPos x y) =~ (EqPos x' y') =
     vmag (cartesian (HorPos x y, 1) - cartesian (HorPos x' y', 1)) < d
    where
      d = 1e-4

instance AEq Vector3 where
  v =~ w = vmag (v - w) < d
    where
      d = 1.0e-8 :: Double

instance AEq Float where
  x =~ y = abs (x - y) < (1.0e-4 :: Float)

instance (AEq a) => AEq (Radians a) where
  (Radians x) =~ (Radians y) = x =~ y

{--| Given cos A and sin A solve A --}
solveAngle :: Double -> Double -> Radians Double
solveAngle c s = solveAngle' (cutoff c) (cutoff s)

solveAngle' :: Double -> Double -> Radians Double
solveAngle' c s
  | s > 0 = arccosine c
  | c > 0 = Radians (2 * pi) + arcsine s
  | otherwise = Radians (2 * pi) - arccosine c

cutoff :: Double -> Double
cutoff d | d < -1 = -1
         | d > 1 = 1
         | otherwise = d

cartesian :: (HorPos, Double) -> Vector3
cartesian (HorPos az al, r) = Vector3
  { v3x = r * sine incl * cosine az
  , v3y = r * sine incl * sine az
  , v3z = r * cosine incl
  }
  where
    incl = Degrees 90 - al

polair :: Vector3 -> (HorPos, Double)
polair v = (HorPos (degrees (solveAngle cosfi sinfi)) al, r)
  where
    r = vmag v
    incl = arccosine (v3z v/r)
    cosfi = v3x v/ sqrt (v3x v * v3x v + v3y v * v3y v)
    sinfi = v3y v/ sqrt (v3x v * v3x v + v3y v * v3y v)
    al = 90 - incl
