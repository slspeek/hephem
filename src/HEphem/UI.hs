module HEphem.UI where 

import HEphem.HEphem 
import HEphem.BSParser
import qualified Graphics.Gloss.Data.Point as P
import           GHC.Float
import           Control.Monad
import           Control.Arrow
import           Data.Maybe
import           Test.QuickCheck
import           Data.Vector.Transform.T3
import           Data.Vector.V3
import           Data.Vector.Fancy
import           Data.Vector.Class 
import           Data.Vector.Transform.Fancy
import Data.Angle

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (> 1))

{-- For graphical representation --}
data World = World { wStars :: [BrightStar], wScreen :: Screen }
  deriving (Eq, Show)

{-- Viewing screen has a direction and distance --}
data Screen = Screen HorPos Double
  deriving (Eq, Show)

cartesian :: HorPos -> Vector3
cartesian (HorPos az al) = Vector3
  { v3x = sine incl * cosine az
  , v3y = sine incl * sine az
  , v3z = cosine incl
  }
  where
    incl = Degrees 90 - al

screenCoord :: Screen -> HorPos -> Maybe P.Point
screenCoord s (HorPos az h)
  | h > 0 =
      let v = screenIntersect s (HorPos az h)
      in case v of
        Just p  -> relativeCoord s p
        Nothing -> Nothing
  | otherwise = Nothing

relativeCoord :: Screen -> Vector3 -> Maybe P.Point
relativeCoord s w = fmap (double2Float *** double2Float) (solveLinearEq (grid s) v)
  where
    v = w - origin s

solveLinearEq :: (Vector3, Vector3) -> Vector3 -> Maybe (Scalar, Scalar)
solveLinearEq (v, w) a = listToMaybe [z | z <- nums
                                        , dis z < 0.1]
  where
    dis (x, y) = vmag $ (x *| v + y *| w) - a
    nums = [(x, y) | (x, y) <- l
                   , isNum x
                   , isNum y]
    l = [useToSolve accA accB (v, w) a | accA <- [v3x, v3y, v3z]
                                       , accB <- [v3x, v3y, v3z]
                                       , accA dum /= accB dum]
    dum = Vector3 0 1 2
    isNum x = not (isNaN x) && not (isInfinite x)

useToSolve :: Fractional t1 => (t -> t1) -> (t -> t1) -> (t, t) -> t -> (t1, t1)
useToSolve accA accB (v, w) a = (p, q)
  where
    q = (accB a * accA v - accA a * accB v) / (accB w * accA v + accA w * accB v)
    p = (accA a - q * accA w) / accA v

origin :: Screen -> Vector3
origin (Screen vdir dist) =
  let snv = cartesian vdir
  in vnormalise snv |* dist

normalVector :: Screen -> Vector3
normalVector (Screen vdir _) = cartesian vdir

grid :: Screen -> (Vector3, Vector3)
grid (Screen (HorPos az al) _) = (r x, r y)
  where
    x = Vector3 0 1 0
    y = Vector3 0 0 1
    r1 = rotateT AxisX AxisZ (radians al)
    r2 = rotateT AxisX AxisY (radians az)
    r v = transformP3 r2 (transformP3 r1 v)

screenIntersect :: Screen -> HorPos -> Maybe Vector3
screenIntersect s hor = if ln /= 0 && f > 0
                          then Just $ f *| lv
                          else Nothing
  where
    lv = cartesian hor
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln
    
