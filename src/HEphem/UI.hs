{-# LANGUAGE TemplateHaskell #-}


module HEphem.UI where

import           Control.Arrow
import           Control.Lens                     hiding (element)
import           Control.Monad
import           Data.Angle
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Time.Clock
import           Data.Vector.Class
import           Data.Vector.Fancy
import           Data.Vector.Transform.Fancy
import           Data.Vector.Transform.T3
import           Data.Vector.V3
import           GHC.Float
import qualified Graphics.Gloss.Data.Point        as P
import           Graphics.Gloss.Interface.IO.Game
import           HEphem.Data
import           HEphem.HEphem
import           Test.QuickCheck

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (> 1))

-- | World in the Gloss Game sense
data World = World
  { _wObjects :: [SkyObject]
  , _wScreen  :: Screen
  , _wGeo     :: GeoLoc
  , _wLlc     :: (Int, Int)
  }
  deriving (Show)

{-- Viewing screen has a direction and distance --}
data Screen = Screen
  { _sDirection :: HorPos
  , _sDistance  :: Double
  }
  deriving (Eq, Show)

makeLenses ''World
makeLenses ''Screen

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
screenIntersect s hor = if abs ln > 0.01 && f > 0
                          then Just $ f *| lv
                          else Nothing
  where
    lv = cartesian hor
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln

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
solveLinearEq (v, w) a = if null dlist then
                            Nothing
                         else Just closest
  where
    closest = snd . Map.findMin $ Map.fromList dlist
    dlist = [ (dis z, z) | z <- nums]
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
    q = (accB a * accA v - accA a * accB v) / (accB w * accA v - accA w * accB v)
    p = (accA a - q * accA w) / accA v


pictureSkyObject:: (SkyObject, (Float, Float))-> Picture
pictureSkyObject (so, pos)  = case so of NGCObject{} -> pictureNGCObject so pos
                                         BrightStar{} -> pictureStar so pos


pictureStar:: SkyObject -> (Float,Float) -> Picture
pictureStar s (x, y) = Color white . Translate (10 * x) (10 * y) $ circleSolid (max 1 (6 - magnitude s))

pictureNGCObject:: SkyObject -> (Float,Float) -> Picture
pictureNGCObject n (x, y) = Pictures
  [ Color blue . Translate (10 * x) (10 * y) $ circle (max 1 (6 - magnitude n))
  , Color blue . Translate (10 * x + 10) (10 * y - 10) $ Scale 0.1 0.1 $ text (nMessier n)
  ]

pictureDashboard:: World -> Picture
pictureDashboard w = Color red $ Translate (fromIntegral x + 10) (fromIntegral y + 8) $ Scale 0.1 0.1 $
  Text $ "Azimuth: " ++
  show (view (wScreen .sDirection . hAzimuth) w) ++
  " Altitude: " ++
  show (view (wScreen . sDirection . hAltitude)  w) ++
  " Distance: " ++
  show (view (wScreen . sDistance )w)
  where
    (x, y) = view wLlc w

screenCoordAt:: Screen -> GeoLoc -> UTCTime ->SkyObject-> Maybe(Float,Float)
screenCoordAt scr geo t so = screenCoord scr (snd (horizontal geo t so))

visibleObjects:: World -> UTCTime -> [(SkyObject, (Float,Float))]
visibleObjects w t = mapMaybe f $ view wObjects w
  where
    f so = do
      c <- screenCoordAt (view wScreen w) (view wGeo w) t so
      return (so, c)

pictureWorld :: World -> IO Picture
pictureWorld w =
  do
  utc <- getCurrentTime
  let stars = Pictures $ map pictureSkyObject (visibleObjects w utc)
  return $ Pictures [stars, pictureDashboard w]

starColor :: Color
starColor = white

eventHandler :: Event -> World -> IO World
eventHandler ev w =
  case ev of
    EventKey (SpecialKey KeyLeft) Up _ _  -> return $ over (wScreen . sDirection . hAzimuth) (-3+) w
    EventKey (SpecialKey KeyRight) Up _ _ -> return $ over (wScreen . sDirection . hAzimuth) (+3) w
    EventKey (SpecialKey KeyUp) Up _ _    -> return $ over (wScreen . sDirection . hAltitude) (+3) w
    EventKey (SpecialKey KeyDown) Up _ _  -> return $ over (wScreen . sDirection . hAltitude) (-3+) w
    EventKey (Char 'w') Up _ _            -> return $ over (wScreen . sDistance) (* 1.1) w
    EventKey (Char 's') Up _ _            -> return $ over (wScreen . sDistance) (/ 1.1) w
    EventResize (x, y) -> return $ set wLlc (- x `div` 2, - y `div` 2) w
    _ -> return  w
