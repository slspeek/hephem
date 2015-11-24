module HEphem.Data where

import           Data.Angle
import           Test.QuickCheck

import           Data.Vector.V3
import           Data.Vector.Class ()

type Deg = Degrees Double

data EqPos = EqPos { eRA,eDec :: Deg }
  deriving (Eq, Show)

data BrightStar =
       BrightStar
         { bName :: String
         , bHRNo :: Int
         , bRA :: Deg
         , bDec :: Deg
         , bNotes :: String
         , bMagitude :: Double
         , bUminB :: Maybe Double
         , bBminV :: Double
         , bSpectralType :: String
         }
  deriving (Eq, Show)

bEquatorial :: BrightStar -> EqPos
bEquatorial b = EqPos (bRA b) (bDec b)

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

instance AEq HorPos where
  x =~ y = abs (hAzimuth x - hAzimuth y) < d && abs (hAltitude x - hAltitude y) < d
    where
      d = 0.1 :: Degrees Double

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
