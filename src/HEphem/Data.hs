{-# LANGUAGE ExistentialQuantification #-}

module HEphem.Data where

import           Data.Angle
import           Test.QuickCheck
import           Data.Vector.Class ()
import           Data.Vector.V3

type Deg = Degrees Double

data EqPos = EqPos { eRA,eDec :: Deg }
  deriving (Eq, Show)

data SkyObject =
  NGCObject
  { nID :: String
  , nPGC:: String
  , nMessier:: String
  , nType:: String
  , nClass:: String
  , nRA :: Deg
  , nDec :: Deg
  , nMag :: Float
   }
  |BrightStar
            { bName         :: String
            , bHRNo         :: Int
            , bRA           :: Deg
            , bDec          :: Deg
            , bNotes        :: String
            , bMagitude     :: Float
            , bUminB        :: Maybe Float
            , bBminV        :: Float
            , bSpectralType :: String
            }
            deriving (Show)

equatorial:: SkyObject -> EqPos
equatorial (BrightStar _ _ r d _ _ _ _ _) = EqPos r d
equatorial (NGCObject _ _ _ _ _ r d _ ) = EqPos r d

magnitude:: SkyObject -> Float
magnitude (BrightStar _ _ _ _ _ m _ _ _) = m
magnitude (NGCObject _ _ _ _ _ _ _ m ) = m

sType:: SkyObject -> String
sType BrightStar{} = "Star"
sType (NGCObject _ _ _ t _ _ _ _ ) = t

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

instance AEq HorPos where
  x =~ y = abs (hAzimuth x - hAzimuth y) < d && abs (hAltitude x - hAltitude y) < d
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

data GeoLoc = GeoLoc { gLatitude,gLongitude :: Deg }
  deriving (Eq, Show)

data HorPos = HorPos { hAzimuth,hAltitude :: Deg }
  deriving (Eq, Show)

instance Arbitrary HorPos where
  arbitrary = do
    az <- suchThat arbitrary (\x -> x >= 0 && x <= 360)
    al <- suchThat arbitrary (\x -> x >= 0 && x <= 90)
    return $ HorPos (Degrees az) (Degrees al)
