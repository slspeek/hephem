{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

module HEphem.Data where

import           Data.Angle
import           Test.QuickCheck
import           Data.Vector.V3
import           Control.Lens                     hiding (element)
import           Text.Printf
import           Data.Vector.Class


type Deg = Degrees Double

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

data GeoLoc = GeoLoc { _gLatitude,_gLongitude :: Deg }
  deriving (Eq, Show)

makeLenses ''GeoLoc

data BrightStar = BrightStar
          { bName         :: String
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
equatorial (Star (BrightStar _ _ r d _ _ _ _ _)) = EqPos r d
equatorial (NGC (NGCObject _ _ _ _ _ r d _ )) = EqPos r d

magnitude:: SkyObject -> Float
magnitude (Star a) = bMag a
magnitude (NGC  a) = nMag a

description :: SkyObject -> String
description (Star(BrightStar n hr _ _ _ m _ _ _)) = printf "%s HR# %v Mag %.1f" n hr m
description (NGC (NGCObject i _ _ t _ _ _ m )) = printf "%s Type %s Mag %.1f" i t m

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

instance AEq HorPos where
  x =~ y = abs (_hAzimuth x - _hAzimuth y) < d && abs (_hAltitude x - _hAltitude y) < d
    where
      d = 0.1 :: Deg

instance AEq EqPos where
  x =~ y = abs (eRA x - eRA y) < d && abs (eDec x - eDec y) < d
    where
      d = 0.1 :: Deg

instance AEq Vector3 where
  v =~ w = abs (v3x v - v3x w) < d &&
           abs (v3y v - v3y w) < d &&
           abs (v3z v - v3z w) < d
    where
      d = 1.0e-8 :: Double

instance AEq Float where
  x =~ y = abs (x - y) < (1.0e-4 :: Float)

instance (AEq a) => AEq (Degrees a) where
  (Degrees x) =~ (Degrees y) = x =~ y

instance (AEq a) => AEq (Radians a) where
  (Radians x) =~ (Radians y) = x =~ y

{--| Given cos A and sin A solve A --}
solveAngle :: Double -> Double -> Radians Double
solveAngle c s
  | s > 0 = arccosine c
  | c > 0 = Radians (2 * pi) + arcsine s
  | otherwise = Radians (2 * pi) - arccosine c


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
