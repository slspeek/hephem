{-# LANGUAGE TemplateHaskell #-}


module HEphem.UI where

import           Control.Arrow
import           Control.Lens                     hiding (element)
import           Control.Monad
import           Data.Angle
import           Data.Fixed                       (mod')
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
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
import           Text.Printf

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (> 1))

-- | World in the Gloss Game sense
data World = World
  { _wObjects     :: [SkyObject]
  , _wScreen      :: Screen
  , _wGeo         :: GeoLoc
  , _wLlc         :: (Int, Int)
  , _wCurrentTime :: UTCTime
  , _wTimeFactor  :: Float
  , _wMinMag      :: Float
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
  let snv = cartesian (vdir, 1)
  in vnormalise snv |* dist

normalVector :: Screen -> Vector3
normalVector (Screen vdir _) = cartesian (vdir, 1)

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
    lv = cartesian (hor, 1)
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln

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


iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%F %T"

fromDeg :: Deg -> Double
fromDeg (Degrees x) = x

pictureDashboard:: World -> Picture
pictureDashboard w = Color red $ Translate (fromIntegral x + 10) (fromIntegral y + 8) $ Scale 0.1 0.1 $
  Text $ printf "Azimuth: %.2f  Altitude: %.2f  Distance: %.2f Time: %s Speed: %.2f  Minimal mag: %.2f"
                (fromDeg (w^.wScreen.sDirection.hAzimuth))
                (fromDeg (w^.wScreen.sDirection.hAltitude))
                (w^.wScreen.sDistance)
                (iso8601  (w^.wCurrentTime))
                (w^.wTimeFactor)
                (w^.wMinMag)
  where
    (x, y) = view wLlc w

screenCoordAt:: Screen -> GeoLoc -> UTCTime ->SkyObject-> Maybe(Float,Float)
screenCoordAt scr geo t so = screenCoord scr (snd (horizontal geo t so))

visibleObjects:: World -> UTCTime -> [(SkyObject, (Float,Float))]
visibleObjects w t = mapMaybe f $ filter (\x -> magnitude x < w^.wMinMag) (w^.wObjects)
  where
    f so = do
      c <- screenCoordAt (w^.wScreen) (w^.wGeo) t so
      return (so, c)

pictureWorld :: World -> Picture
pictureWorld w =
  let stars = Pictures $ map pictureSkyObject (visibleObjects w (w^.wCurrentTime))
  in Pictures [stars, pictureDashboard w]

starColor :: Color
starColor = white

advanceTime:: Float -> World -> World
advanceTime t w = over wCurrentTime (addUTCTime (realToFrac (w^.wTimeFactor * t))) w

dmod :: Deg -> Int -> Deg
dmod (Degrees d) i = Degrees (d `mod'` fromIntegral i)

eventHandler :: Event -> World -> World
eventHandler ev w =
  case ev of
    EventKey (SpecialKey KeyLeft) Up _ _  -> over (wScreen . sDirection . hAzimuth) (\x -> (x-3) `dmod` 360) w
    EventKey (SpecialKey KeyRight) Up _ _ -> over (wScreen . sDirection . hAzimuth) (\x -> (x+3) `dmod` 360) w
    EventKey (SpecialKey KeyUp) Up _ _    -> over (wScreen . sDirection . hAltitude) (+3) w
    EventKey (SpecialKey KeyDown) Up _ _  -> over (wScreen . sDirection . hAltitude) (-3+) w
    EventKey (Char 'w') Up _ _            -> over (wScreen . sDistance) (* 1.1) w
    EventKey (Char 's') Up _ _            -> over (wScreen . sDistance) (/ 1.1) w
    EventKey (Char 'e') Up _ _            -> over wTimeFactor (* 2) w
    EventKey (Char 'd') Up _ _            -> over wTimeFactor (/ 2) w
    EventKey (Char 'r') Up _ _            -> over wMinMag (+1) w
    EventKey (Char 'f') Up _ _            -> over wMinMag (+ (-1)) w

    EventResize (x, y) ->  set wLlc (- x `div` 2, - y `div` 2) w
    _ ->   w
